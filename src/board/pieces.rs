use std::fmt::Display;

use crate::board::Vec2i;

use super::{moves::PromotionPiece, Direction, Pos, SlidingRay};

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

	pub fn with_pos(self, pos: Pos) -> PositionedPiece
	{
		PositionedPiece::new(pos, self)
	}

	pub fn from_char(fen_char: char) -> Option<Self>
	{
		let piece_type = match fen_char
		{
			'p' | 'P' => PieceType::Pawn,
			'n' | 'N' => PieceType::Knight,
			'b' | 'B' => PieceType::Bishop,
			'r' | 'R' => PieceType::Rook,
			'q' | 'Q' => PieceType::Queen,
			'k' | 'K' => PieceType::King,
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
	pub fn is_king(self) -> bool
	{
		self.piece_type == PieceType::King
	}

	pub fn promote_to(self, promotion: PromotionPiece) -> Self
	{
		Self::new(self.color, promotion.into())
	}

	pub fn is_sliding(self) -> bool
	{
		self.sliding_axis().is_some()
	}

	pub fn sliding_axis(self) -> Option<SlidingAxis>
	{
		match self.piece_type
		{
			PieceType::Rook => Some(SlidingAxis::Orthogonal),
			PieceType::Bishop => Some(SlidingAxis::Diagonal),
			PieceType::Queen => Some(SlidingAxis::Both),
			_ => None,
		}
	}

	pub fn can_slide_in_direction(self, direction: Direction) -> bool
	{
		self.sliding_axis().map_or(false, |axis| {
			axis == SlidingAxis::Both || axis == direction.axis()
		})
	}

	pub fn forward_vector(self) -> Vec2i
	{
		self.color.forward_vector()
	}
}
impl Display for Piece
{
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
	{
		let mut character = match self.piece_type
		{
			PieceType::Bishop => 'B',
			PieceType::King => 'K',
			PieceType::Knight => 'N',
			PieceType::Pawn => 'P',
			PieceType::Queen => 'Q',
			PieceType::Rook => 'R',
		};
		if self.color == Color::Black
		{
			character = character.to_ascii_lowercase();
		}
		write!(f, "{character}")
	}
}
impl From<PositionedPiece> for Piece
{
	fn from(value: PositionedPiece) -> Self
	{
		value.1
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PositionedPiece(pub Pos, pub Piece);

impl PositionedPiece
{
	pub fn new(pos: Pos, piece: Piece) -> Self
	{
		Self(pos, piece)
	}

	pub fn pos(self) -> Pos
	{
		self.0
	}

	pub fn color(self) -> Color
	{
		self.1.color
	}
	pub fn piece_type(self) -> PieceType
	{
		self.1.piece_type
	}

	pub fn is_pawn(self) -> bool
	{
		self.1.is_pawn()
	}
	pub fn is_king(self) -> bool
	{
		self.1.is_king()
	}

	pub fn is_sliding(self) -> bool
	{
		self.1.is_sliding()
	}
	pub fn sliding_axis(self) -> Option<SlidingAxis>
	{
		self.1.sliding_axis()
	}

	pub fn can_slide_in_direction(self, direction: Direction) -> bool
	{
		self.1.can_slide_in_direction(direction)
	}
	pub fn forward_vector(self) -> Vec2i
	{
		self.1.forward_vector()
	}

	pub fn sliding_ray(self, direction: Direction) -> Option<SlidingRay>
	{
		self.can_slide_in_direction(direction)
			.then(|| SlidingRay::new(self.pos(), direction))
	}
}
impl Display for PositionedPiece
{
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
	{
		write!(f, "{}{}", self.1, self.0)
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
impl Display for PieceType
{
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
	{
		write!(
			f,
			"{}",
			match self
			{
				Self::Pawn => "",
				Self::Knight => "N",
				Self::Bishop => "B",
				Self::Rook => "R",
				Self::Queen => "Q",
				Self::King => "K",
			}
		)
	}
}
impl From<PromotionPiece> for PieceType
{
	fn from(value: PromotionPiece) -> Self
	{
		match value
		{
			PromotionPiece::Queen => Self::Queen,
			PromotionPiece::Rook => Self::Rook,
			PromotionPiece::Bishop => Self::Bishop,
			PromotionPiece::Knight => Self::Knight,
		}
	}
}
impl PieceType
{
	pub fn with_color(self, color: Color) -> Piece
	{
		Piece {
			piece_type: self,
			color,
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Color
{
	White,
	Black,
}
impl Color
{
	pub fn forward_vector(self) -> Vec2i
	{
		match self
		{
			Self::White => Vec2i::new(0, 1),
			Self::Black => Vec2i::new(0, -1),
		}
	}

	pub fn char(self) -> char
	{
		match self
		{
			Self::White => 'w',
			Self::Black => 'b',
		}
	}
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SlidingAxis
{
	Orthogonal,
	Diagonal,
	Both,
}
impl SlidingAxis
{
	const ORTHOGONAL_DIRECTIONS: [Direction; 4] = [
		Direction::North,
		Direction::East,
		Direction::South,
		Direction::West,
	];
	const DIAGONAL_DIRECTIONS: [Direction; 4] = [
		Direction::Northeast,
		Direction::Southeast,
		Direction::Southwest,
		Direction::Northwest,
	];
	const ALL_DIRECTIONS: [Direction; 8] = [
		Direction::North,
		Direction::Northeast,
		Direction::East,
		Direction::Southeast,
		Direction::South,
		Direction::Southwest,
		Direction::West,
		Direction::Northwest,
	];

	pub fn orthogonal_allowed(self) -> bool
	{
		self == Self::Orthogonal || self == Self::Both
	}
	pub fn diagonal_allowed(self) -> bool
	{
		self == Self::Diagonal || self == Self::Both
	}

	pub fn allowed_directions(self) -> impl Iterator<Item = &'static Direction>
	{
		match self
		{
			Self::Orthogonal => Self::ORTHOGONAL_DIRECTIONS.iter(),
			Self::Diagonal => Self::DIAGONAL_DIRECTIONS.iter(),
			Self::Both => Self::ALL_DIRECTIONS.iter(),
		}
	}

	pub fn rays_from(self, pos: Pos) -> impl Iterator<Item = SlidingRay>
	{
		self.allowed_directions()
			.map(move |dir| SlidingRay::new(pos, *dir))
	}
}

#[cfg(test)]
mod test
{
	use crate::board::Direction;

	use super::{Color, Piece, PieceType, SlidingAxis};

	#[test]
	fn piece_slide_ability_check()
	{
		let queen = Piece::new(Color::White, PieceType::Queen);
		let rook = Piece::new(Color::White, PieceType::Rook);
		let bishop = Piece::new(Color::White, PieceType::Bishop);

		queen.can_slide_in_direction(Direction::Southeast);

		SlidingAxis::Both
			.allowed_directions()
			.all(|dir| queen.can_slide_in_direction(*dir));

		SlidingAxis::Diagonal
			.allowed_directions()
			.all(|dir| bishop.can_slide_in_direction(*dir));
		SlidingAxis::Diagonal
			.allowed_directions()
			.all(|dir| !rook.can_slide_in_direction(*dir));

		SlidingAxis::Orthogonal
			.allowed_directions()
			.all(|dir| rook.can_slide_in_direction(*dir));
		SlidingAxis::Orthogonal
			.allowed_directions()
			.all(|dir| !bishop.can_slide_in_direction(*dir));
	}
}
