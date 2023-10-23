#![recursion_limit = "512"]

use anyhow::{anyhow, Result};
use clap::Parser;
use enum_iterator::{all, Sequence};
use itertools::{Itertools, Position};
use rand::{rngs::StdRng, RngCore as _, SeedableRng as _};
use std::{
    collections::BTreeMap,
    fmt::Write,
    io::{BufRead, BufReader},
    path::{Path, PathBuf},
    time::Instant,
};

const TEXT_INSIDE_SPACING: &str = "   ";

#[derive(Parser)]
struct Opts {
    /// Puzzle width.  Defaults to the longest word
    #[arg(long)]
    width: Option<usize>,

    /// Puzzle height. Defaults to the puzzle width.
    #[arg(long)]
    height: Option<usize>,

    /// Maximum computation time.  Longer allows for potentially more-complex
    /// puzzles.
    #[arg(long, default_value = "1s")]
    time_limit: humantime::Duration,

    /// Seed to regenerate a puzzle
    #[arg(long)]
    seed: Option<String>,

    /// Output type
    #[arg(long, default_value = "text")]
    format: OutputFormat,

    /// Word list file (or `-` for stdin)
    words: PathBuf,
}

#[derive(Default, Clone, clap::ValueEnum)]
enum OutputFormat {
    #[default]
    Text,
    Html,
}

fn main() -> Result<()> {
    let opts = Opts::parse();

    let (title, wl) = get_wordlist(&opts.words)?;
    if wl.is_empty() {
        return Err(anyhow!("empty word list"));
    }

    let start_time = Instant::now();
    let mut iterations = 0;

    let width = opts
        .width
        .unwrap_or_else(|| wl.iter().map(|word| word.len()).max().unwrap());
    let height = opts.height.unwrap_or(width);

    let seed = opts
        .seed
        .map(hex::decode)
        .transpose()
        .map_err(|decode_err| anyhow!("Decoding hex seed: {decode_err}"))?
        .map(|bytes| {
            (bytes.len() == 32)
                .then_some(bytes)
                .ok_or_else(|| anyhow!("seed is not 32-bytes long"))
        })
        .transpose()?
        .map(|seed_bytes| <[u8; 32]>::try_from(seed_bytes.as_slice()).unwrap());

    let mut puzzle = if let Some(seed) = seed {
        Puzzle::new(height, width, Some(seed)).with_words(&wl.words)
    } else {
        let mut best_puzzle: Option<Puzzle> = None;
        while start_time.elapsed() < opts.time_limit.into() {
            iterations += 1;
            let puzzle = Puzzle::new(height, width, None).with_words(&wl.words);

            if let Some(best_puzzle) = &mut best_puzzle {
                if puzzle.score >= best_puzzle.score {
                    *best_puzzle = puzzle;
                }
            } else {
                best_puzzle = Some(puzzle);
            }
        }

        best_puzzle.ok_or_else(|| {
            anyhow!("Couldn't build a puzzle with a reasonable score. Try a larger grid.")
        })?
    };

    puzzle.fill();

    eprintln!(
        "Best grid score out of {iterations} iterations: {}",
        puzzle.score
    );

    match opts.format {
        OutputFormat::Text => {
            println!();
            println!("{puzzle}");
            eprintln!();
            eprintln!("Seed: {}", hex::encode(puzzle.seed));
            eprintln!("Key:");
            eprintln!("{}", puzzle.word_key);
        }
        OutputFormat::Html => {
            use html::tables::{Table, TableCell, TableRow};

            let mut doc = html::root::Html::builder();
            let mut body = html::root::Body::builder();
            let title = title.as_deref().unwrap_or("Word Search").to_string();
            doc.style("font-family: Arial");
            body.push(
                html::content::Heading1::builder()
                    .style("text-align: center")
                    .text(title)
                    .build(),
            );

            let mut puzzle_table = Table::builder();
            puzzle_table
                .style(r#"font-family: Monaco, monospace; font-size: 20px; border: 1px solid; margin-left: auto; margin-right: auto; margin-top: 40px; padding: 20px"#);
            for row in &puzzle.rows {
                let mut html_row = TableRow::builder();
                html_row.style("height: 30px");
                for space in &row.spaces {
                    html_row.push(
                        TableCell::builder()
                            .text(format!("{space}"))
                            .style("text-align: center; width: 30px")
                            .build(),
                    );
                }
                puzzle_table.push(html_row.build());
            }

            body.push(puzzle_table.build());

            body.push(
                html::content::Heading1::builder()
                    .style("padding-top: 20px; text-align: center")
                    .text("Word List")
                    .build(),
            );
            let mut words_table = Table::builder();
            words_table.style("font-family: Monaco, monospace, font-size: 20px; margin-left: auto; margin-right: auto; border: 0px solid black");
            let mut words_table_row = TableRow::builder();
            for words_in_col in &puzzle
                .word_list
                .iter()
                .chunks((puzzle.word_list.len() + 2) / 3)
            {
                let mut word_col = TableCell::builder();
                word_col
                    .style("font-family: Monaco, monospace; font-size: 14px; vertical-align: top; padding-right: 2em; padding-left: 2em");
                for word in words_in_col {
                    word_col
                        // .text("&bull; ")
                        .text(word.to_string())
                        .text("</br>");
                }
                words_table_row.push(word_col.build());
            }
            words_table.push(words_table_row.build());
            body.push(words_table.build());

            doc.push(body.build());
            print!("{}", doc.build());
        }
    }

    Ok(())
}

fn get_wordlist(path: &Path) -> Result<(Option<String>, WordList), std::io::Error> {
    let input = if path.to_str().unwrap() == "-" {
        Box::new(BufReader::new(std::io::stdin())) as Box<dyn BufRead>
    } else {
        Box::new(BufReader::new(std::fs::File::open(path)?)) as Box<dyn BufRead>
    };

    let mut words = vec![];
    let mut title = None::<String>;
    for line in input.lines() {
        let line = line?;
        if let Some(s) = line.strip_prefix('#') {
            title = Some(s.to_owned());
        } else {
            words.push(line);
        }
    }

    Ok((title, WordList { words }))
}

struct Puzzle {
    rows: Vec<Row>,
    seed: [u8; 32],
    rng: StdRng,
    score: usize,
    word_list: WordList,
    word_key: WordKey,
}

#[derive(Default)]
struct WordKey(BTreeMap<String, WordLocation>);

impl std::ops::Deref for WordKey {
    type Target = BTreeMap<String, WordLocation>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for WordKey {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl std::fmt::Display for WordKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (
            word,
            WordLocation {
                start_row,
                start_col,
                direction,
            },
        ) in self.0.iter()
        {
            writeln!(
                f,
                "{word}: (row {}, col {}, {direction})",
                start_row + 1,
                start_col + 1,
            )?
        }

        Ok(())
    }
}

struct WordLocation {
    start_row: usize,
    start_col: usize,
    direction: Direction,
}

impl std::fmt::Display for Puzzle {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (position, row) in self.rows.iter().with_position() {
            if matches!(position, Position::Middle | Position::Last) {
                writeln!(f)?;
            }
            writeln!(f, "{row}")?;
        }
        writeln!(f)?;
        writeln!(f, "Word List ({}):", self.word_list.len())?;
        writeln!(f)?;
        let n_columns =
            (self.width() * (1 + TEXT_INSIDE_SPACING.len())) / (self.word_list.max_word_len() + 3);
        self.word_list.to_text_columns(f, n_columns)?;
        Ok(())
    }
}

impl Puzzle {
    pub fn new(height: usize, width: usize, seed: Option<[u8; 32]>) -> Self {
        let seed: [u8; 32] = seed.unwrap_or_else(|| {
            let mut seed = [0u8; 32];
            getrandom::getrandom(&mut seed).unwrap();
            seed
        });
        let rng = rand::rngs::StdRng::from_seed(seed);
        Self {
            rows: vec![Row::new(width); height],
            seed,
            rng,
            score: 0,
            word_list: WordList::default(),
            word_key: Default::default(),
        }
    }

    pub fn with_words<S: AsRef<str>>(mut self, words: impl IntoIterator<Item = S>) -> Self {
        let mut n_missing = 0;
        for word in words {
            if !self.add_word(word.as_ref()) {
                n_missing += 1;
            }
        }
        self.score = self.score.saturating_sub(n_missing);
        self
    }

    pub fn space(&self, row_no: usize, col_no: usize) -> Option<Space> {
        self.rows
            .get(row_no)
            .and_then(|row| row.spaces.get(col_no).cloned())
    }

    pub fn add_word(&mut self, word: &str) -> bool {
        let mut best_score = 0;
        let mut scores = vec![];
        let word = word.to_ascii_uppercase();

        let first_char = word.chars().next().unwrap();
        for (row_no, row) in self.rows.iter().enumerate() {
            for (col_no, space) in row.spaces.iter().enumerate() {
                if matches!(space, Space::Filled(c) if *c != first_char) {
                    // Already filled with wrong character
                    continue;
                }
                for dir in all::<Direction>() {
                    if let Ok(score) = self.measure(row_no, col_no, dir, &word) {
                        if score > best_score {
                            scores.clear();
                        }
                        best_score = score;
                        scores.push((row_no, col_no, dir));
                    }
                }
            }
        }

        if scores.is_empty() {
            // Couldn't place word
            return false;
        }

        let idx = self.rng.next_u32() % scores.len() as u32;
        let (start_row, start_col, direction) = scores.get(idx as usize).unwrap();

        for (char_no, c) in word.char_indices() {
            let (dy, dx) = (*direction).into();
            let this_row_no = start_row.checked_add_signed(dy * char_no as isize).unwrap();
            let this_col_no = start_col.checked_add_signed(dx * char_no as isize).unwrap();
            let space = self
                .rows
                .get_mut(this_row_no)
                .and_then(|row| row.spaces.get_mut(this_col_no))
                .unwrap();
            *space = Space::Filled(c);
        }
        self.word_list.push(word.clone());
        self.word_key.insert(
            word,
            WordLocation {
                start_row: *start_row,
                start_col: *start_col,
                direction: *direction,
            },
        );

        self.score += best_score + 1;

        true
    }

    pub fn width(&self) -> usize {
        self.rows.first().unwrap().spaces.len()
    }

    pub fn measure(
        &self,
        start_row: usize,
        start_col: usize,
        dir: Direction,
        word: &str,
    ) -> Result<usize, MeasureError> {
        let mut score = 0;

        let (dy, dx) = dir.into();

        // Work from the end so that we can quickly fail if it doesn't fit
        for (char_no, c) in word.char_indices().rev() {
            let this_row_no = start_row
                .checked_add_signed(dy * char_no as isize)
                .ok_or(MeasureError::DoesntFit)?;
            let this_col_no = start_col
                .checked_add_signed(dx * char_no as isize)
                .ok_or(MeasureError::DoesntFit)?;
            match self.space(this_row_no, this_col_no) {
                Some(space) => match space {
                    Space::Blank(_) => (),
                    Space::Filled(space_c) if space_c == c => {
                        score += 1;
                    }
                    _ => return Err(MeasureError::Conflicts),
                },
                None => return Err(MeasureError::DoesntFit),
            }
        }

        Ok(score)
    }

    pub fn fill(&mut self) {
        let mut rng = self.rng.clone();
        for row in &mut self.rows {
            for space in &mut row.spaces {
                if matches!(space, Space::Blank(_)) {
                    let letter = rand_letter(&mut rng);
                    *space = Space::Blank(Some(letter));
                }
            }
        }
    }
}

#[derive(Default)]
struct WordList {
    words: Vec<String>,
}

impl std::ops::Deref for WordList {
    type Target = Vec<String>;

    fn deref(&self) -> &Self::Target {
        &self.words
    }
}

impl std::ops::DerefMut for WordList {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.words
    }
}

impl WordList {
    pub fn to_text_columns(
        &self,
        mut f: impl std::fmt::Write,
        n_columns: usize,
    ) -> Result<(), std::fmt::Error> {
        let max_word_len = self.max_word_len();
        let words_per_col = self.words.len() / n_columns;
        for row_no in 0..=self.words.len() / n_columns {
            for col_no in 0..n_columns {
                if let Some(word) = self.words.get(col_no * words_per_col + row_no + col_no) {
                    write!(f, "* {word:-max_word_len$}  ")?
                }
            }
            writeln!(f)?;
        }

        Ok(())
    }

    pub fn max_word_len(&self) -> usize {
        self.words.iter().map(|s| s.len()).max().unwrap()
    }
}

#[derive(Clone)]
struct Row {
    spaces: Vec<Space>,
}

impl std::fmt::Display for Row {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (position, c) in self.spaces.iter().with_position() {
            match position {
                Position::First | Position::Only => write!(f, "{c}")?,
                Position::Middle | Position::Last => write!(f, "{TEXT_INSIDE_SPACING}{c}")?,
            }
        }
        Ok(())
    }
}

impl Row {
    pub fn new(width: usize) -> Self {
        Self {
            spaces: vec![Space::default(); width],
        }
    }
}

#[derive(Clone)]
enum Space {
    Blank(Option<char>),
    Filled(char),
}

impl Default for Space {
    fn default() -> Self {
        Self::Blank(None)
    }
}

impl std::fmt::Display for Space {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Space::Blank(None) => f.write_char('-'),
            Space::Blank(Some(c)) => f.write_char(*c),
            Space::Filled(c) => f.write_char(*c),
        }
    }
}

fn rand_letter(rng: &mut StdRng) -> char {
    let cap_a: u32 = 'A'.into();
    char::from_u32(cap_a + (rng.next_u32() % 26)).unwrap()
}

#[derive(Debug)]
enum MeasureError {
    DoesntFit,
    Conflicts,
}

#[derive(Clone, Copy, Debug, PartialEq, Sequence)]
enum Direction {
    UpLeft,
    Up,
    UpRight,
    Left,
    Right,
    DownLeft,
    DownRight,
}

impl std::fmt::Display for Direction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

impl From<Direction> for (isize, isize) {
    fn from(value: Direction) -> Self {
        match value {
            Direction::UpLeft => (-1, -1),
            Direction::Up => (-1, 0),
            Direction::UpRight => (-1, 1),
            Direction::Left => (0, -1),
            Direction::Right => (0, 1),
            Direction::DownLeft => (1, -1),
            Direction::DownRight => (1, 1),
        }
    }
}
