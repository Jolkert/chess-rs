use std::fmt::Display;

use crate::board::Vec2i;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Piece
{
	pub piece_type: PieceType,
	pub color: Color,
}
impl Piece
{
	pub fn new(color: Color, piece_type: PieceType) -> Self
	{
		Self { piece_type, color }
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
			Color::White
		}
		else
		{
			Color::Black
		};

		Some(Self::new(color, piece_type))
	}

	pub fn is_pawn(self) -> bool
	{
		self.piece_type == PieceType::Pawn
	}

	pub fn forward_vector(self) -> Vec2i
	{
		if self.color == Color::White
		{
			Vec2i::new(0, 1)
		}
		else
		{
			Vec2i::new(0, -1)
		}
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
pub enum Color
{
	White,
	Black,
}
impl Display for Color
{
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
	{
		write!(
			f,
			"{}",
			match self
			{
				Self::White => "White",
				Self::Black => "Black",
			}
		)
	}
}
impl std::ops::Not for Color
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct WhiteBlackPair<T>
{
	white: T,
	black: T,
}
impl<T> WhiteBlackPair<T>
{
	pub fn new(white: T, black: T) -> Self
	{
		Self { white, black }
	}
}
impl<T> std::ops::Index<Color> for WhiteBlackPair<T>
{
	type Output = T;

	fn index(&self, index: Color) -> &Self::Output
	{
		match index
		{
			Color::White => &self.white,
			Color::Black => &self.black,
		}
	}
}
impl<T> std::ops::IndexMut<Color> for WhiteBlackPair<T>
{
	fn index_mut(&mut self, index: Color) -> &mut Self::Output
	{
		match index
		{
			Color::White => &mut self.white,
			Color::Black => &mut self.black,
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SlidingAxis
{
	Orthogonal,
	Diagonal,
	Both,
}
impl SlidingAxis
{
	pub fn orthogonal_allowed(self) -> bool
	{
		self == Self::Orthogonal || self == Self::Both
	}
	pub fn diagonal_allowed(self) -> bool
	{
		self == Self::Diagonal || self == Self::Both
	}
}
