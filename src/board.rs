use std::fmt::Display;

use eframe::egui::Vec2;
use lazy_regex::Regex;

#[derive(Debug)]
pub struct Board
{
	// TODO:
	// 1. castling options
	// 2. en passant
	// 3. legal move checking
	pieces: [Option<Piece>; 64],
	pub to_move: PieceColor,
}
impl Default for Board
{
	fn default() -> Self
	{
		Self {
			pieces: [None; 64],
			to_move: PieceColor::White,
		}
	}
}
impl std::ops::Index<usize> for Board
{
	type Output = Option<Piece>;

	fn index(&self, index: usize) -> &Self::Output
	{
		&self.pieces[index]
	}
}
impl std::ops::IndexMut<usize> for Board
{
	fn index_mut(&mut self, index: usize) -> &mut Self::Output
	{
		&mut self.pieces[index]
	}
}
impl std::ops::Index<BoardPos> for Board
{
	type Output = Option<Piece>;

	fn index(&self, index: BoardPos) -> &Self::Output
	{
		&self.pieces[index.index()]
	}
}
impl std::ops::IndexMut<BoardPos> for Board
{
	fn index_mut(&mut self, index: BoardPos) -> &mut Self::Output
	{
		&mut self.pieces[index.index()]
	}
}

fn fen_regex() -> &'static Regex
{
	lazy_regex::regex!(
		r"(?<pieces>(?:[pnrbkqPNRBKQ1-8]+/){7}[pnrbkqPNRBKQ1-8]+)\s+(?<to_move>[wb])\s+(?<castle>\-|[KQkq]+)\s(?<en_passant>\-|[a-h][36])\s(?<halfmove>\d+)\s(?<fullmove>\d+)"
	)
}

impl Board
{
	pub fn from_fen_string(fen: &str) -> Option<Self>
	{
		let captures = fen_regex().captures(fen)?;

		let (mut cursor_file, mut cursor_rank) = (0, 0);
		let mut pieces: [Option<Piece>; 64] = [None; 64];
		for ch in captures[1].chars()
		{
			if ch == '/'
			{
				cursor_rank += 1;
				cursor_file = 0;
			}
			else if let Some(skip) = ch.to_digit(10)
			{
				cursor_file += skip;
			}
			else
			{
				pieces[(cursor_rank * 8 + cursor_file) as usize] = Piece::from_char(ch);
				cursor_file += 1;
			}
		}

		let to_move = match captures[2].chars().nth(0)?
		{
			'w' => Some(PieceColor::White),
			'b' => Some(PieceColor::Black),
			_ => None,
		}?;

		Some(Self { pieces, to_move })
	}

	pub fn set_piece_at(&mut self, index: usize, piece: Option<Piece>) -> &mut Self
	{
		self.pieces[index] = piece;
		self
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BoardPos
{
	tile_index: u8,
}
impl From<Vec2> for BoardPos
{
	fn from(value: Vec2) -> Self
	{
		Self {
			tile_index: 8 * (value.y.clamp(0.0, 8.0) as u8)
				+ (value.x.clamp(0.0, 8.0) as u8).clamp(0, 7),
		}
	}
}
impl BoardPos
{
	pub const fn from_index(tile_index: usize) -> Self
	{
		Self {
			tile_index: tile_index as u8,
		}
	}
	pub const fn from_rank_file(rank: u8, file: u8) -> Self
	{
		Self {
			tile_index: 8 * file + rank,
		}
	}

	pub const fn rank(self) -> u8
	{
		7 - (self.tile_index / 8)
	}
	pub const fn top_down_rank(self) -> u8
	{
		self.tile_index / 8
	}
	pub const fn file(self) -> u8
	{
		self.tile_index % 8
	}
	pub const fn rank_file(self) -> (u8, u8)
	{
		(self.rank(), self.file())
	}
	pub const fn index(self) -> usize
	{
		self.tile_index as usize
	}

	pub fn file_char(self) -> char
	{
		char::from_u32(self.file() as u32 + 97).expect("Invalid file character!")
	}
}
impl Display for BoardPos
{
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
	{
		write!(f, "{}{}", self.file_char(), self.rank() + 1)
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Piece
{
	pub piece_type: PieceType,
	pub color: PieceColor,
}
impl Piece
{
	pub fn new(color: PieceColor, piece_type: PieceType) -> Self
	{
		Self { color, piece_type }
	}

	pub fn from_char(fen_char: char) -> Option<Self>
	{
		let piece_type = match fen_char.to_ascii_lowercase()
		{
			'p' => PieceType::Pawn,
			'n' => PieceType::Knight,
			'b' => PieceType::Bishop,
			'r' => PieceType::Rook,
			'q' => PieceType::Queen,
			'k' => PieceType::King,
			_ => return None,
		};
		let color = if fen_char.is_ascii_uppercase()
		{
			PieceColor::White
		}
		else
		{
			PieceColor::Black
		};

		Some(Self::new(color, piece_type))
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PieceType
{
	Pawn,
	Bishop,
	Rook,
	Knight,
	Queen,
	King,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PieceColor
{
	White,
	Black,
}

impl std::ops::Not for PieceColor
{
	type Output = Self;

	fn not(self) -> Self::Output
	{
		match self
		{
			Self::White => Self::Black,
			Self::Black => Self::White,
		}
	}
}
