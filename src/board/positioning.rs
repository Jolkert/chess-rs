use eframe::egui::Vec2;
use std::fmt::Display;

use super::pieces::SlidingAxis;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Pos
{
	tile_index: u8,
}
impl From<Vec2> for Pos
{
	fn from(value: Vec2) -> Self
	{
		Self {
			tile_index: 8 * (value.y.clamp(0.0, 8.0) as u8)
				+ (value.x.clamp(0.0, 8.0) as u8).clamp(0, 7),
		}
	}
}
impl Pos
{
	pub const fn from_index(tile_index: usize) -> Self
	{
		Self {
			tile_index: tile_index as u8,
		}
	}
	/// 0-indexed; ranks from bottom to top
	pub const fn from_file_rank(file: u8, rank: u8) -> Self
	{
		Self {
			tile_index: 8 * (7 - rank) + file,
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
		char::from_u32(u32::from(self.file()) + 97).expect("Invalid file character!")
	}

	pub fn knight_move_squares(self) -> impl Iterator<Item = Self>
	{
		Vec2i::KNIGHT_OFFSETS
			.iter()
			.filter_map(move |offset| self.move_by(*offset))
	}

	pub fn adjoining_squares(self) -> impl Iterator<Item = Self>
	{
		[
			Vec2i::NORTH,
			Vec2i::NORTH_EAST,
			Vec2i::EAST,
			Vec2i::SOUTH_EAST,
			Vec2i::SOUTH,
			Vec2i::SOUTH_WEST,
			Vec2i::WEST,
			Vec2i::NORTH_WEST,
		]
		.iter()
		.filter_map(move |offset| self.move_by(*offset))
	}

	pub fn move_by(self, offset: Vec2i) -> Option<Self>
	{
		let raw_sum = self.raw_add(offset);
		((0..=7).contains(&raw_sum.file) && (0..=7).contains(&raw_sum.rank))
			.then(|| Self::from_file_rank(raw_sum.file as u8, raw_sum.rank as u8))
	}

	// this is terrible name but idk what to call this -2024-04-29
	/// Same as [`move_by`][Pos::move_by], but returns [`None`] if the start end end positions are the same
	pub fn checked_move(self, offset: Vec2i) -> Option<Self>
	{
		self.move_by(offset)
			.and_then(|pos| (self != pos).then_some(pos))
	}

	pub fn offset_from(self, other: Self) -> Vec2i
	{
		Vec2i::new(
			i32::from(self.file()) - i32::from(other.file()),
			i32::from(self.rank()) - i32::from(other.rank()),
		)
	}
	pub fn offset_to(self, other: Self) -> Vec2i
	{
		Vec2i::new(
			i32::from(other.file()) - i32::from(self.file()),
			i32::from(other.rank()) - i32::from(self.rank()),
		)
	}

	pub fn to_left_edge(self) -> Self
	{
		Self::from_file_rank(0, self.rank())
	}
	pub fn to_right_edge(self) -> Self
	{
		Self::from_file_rank(7, self.rank())
	}

	pub fn to_edge_in_direction_of(self, offset: Vec2i) -> Self
	{
		let (horizontal, vertical) = offset.directions();
		let file = match horizontal
		{
			Some(HorizontalDirection::Left) => 0,
			Some(HorizontalDirection::Right) => 7,
			None => self.file(),
		};
		let rank = match vertical
		{
			Some(VerticalDirection::Down) => 0,
			Some(VerticalDirection::Up) => 7,
			None => self.rank(),
		};

		Self::from_file_rank(file, rank)
	}
}

impl Display for Pos
{
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
	{
		write!(f, "{}{}", self.file_char(), self.rank() + 1)
	}
}

impl Pos
{
	fn raw_add(self, rhs: Vec2i) -> Vec2i
	{
		Vec2i::new(
			i32::from(self.file()) + rhs.file,
			i32::from(self.rank()) + rhs.rank,
		)
	}
}
impl std::ops::Add<Vec2i> for Pos
{
	type Output = Self;

	fn add(self, rhs: Vec2i) -> Self::Output
	{
		self.move_by(rhs).expect("Board position out of bounds!")
	}
}
impl std::ops::Sub<Vec2i> for Pos
{
	type Output = Self;

	fn sub(self, rhs: Vec2i) -> Self::Output
	{
		self + (-rhs)
	}
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
enum HorizontalDirection
{
	Left,
	Right,
}
impl HorizontalDirection
{
	fn from_i32(value: i32) -> Option<Self>
	{
		(value < 0)
			.then_some(Self::Left)
			.or_else(|| (value > 0).then_some(Self::Right))
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Vec2i
{
	pub file: i32,
	pub rank: i32,
}
impl Vec2i
{
	pub const NORTH: Self = Self::new(0, 1);
	pub const NORTH_EAST: Self = Self::new(1, 1);
	pub const EAST: Self = Self::new(1, 0);
	pub const SOUTH_EAST: Self = Self::new(1, -1);
	pub const SOUTH: Self = Self::new(0, -1);
	pub const SOUTH_WEST: Self = Self::new(-1, -1);
	pub const WEST: Self = Self::new(-1, 0);
	pub const NORTH_WEST: Self = Self::new(-1, 1);
	pub const KNIGHT_OFFSETS: [Self; 8] = [
		Self::new(2, 1),
		Self::new(2, -1),
		Self::new(-2, 1),
		Self::new(-2, -1),
		Self::new(1, 2),
		Self::new(1, -2),
		Self::new(-1, 2),
		Self::new(-1, -2),
	];

	pub const fn new(file: i32, rank: i32) -> Self
	{
		Self { file, rank }
	}

	pub fn normalized(self) -> Self
	{
		Self::new(self.file.clamp(-1, 1), self.rank.clamp(-1, 1))
	}

	fn directions(self) -> (Option<HorizontalDirection>, Option<VerticalDirection>)
	{
		let normalized = self.normalized();
		(
			HorizontalDirection::from_i32(normalized.file),
			VerticalDirection::from_i32(normalized.rank),
		)
	}

	pub fn is_straight_line(self) -> bool
	{
		self.rank == 0 || self.file == 0 || self.rank.abs() == self.file.abs()
	}

	pub fn compass_direction(self) -> Direction
	{
		let normalized = self.normalized();
		match (normalized.rank, normalized.file)
		{
			(0, 1) => Direction::North,
			(1, 1) => Direction::Northeast,
			(1, 0) => Direction::East,
			(1, -1) => Direction::Southeast,
			(0, -1) => Direction::South,
			(-1, -1) => Direction::Southwest,
			(-1, 0) => Direction::West,
			(-1, 1) => Direction::Northwest,
			_ => panic!("Normalized Vec2i resulted in invalid values!"),
		}
	}
}
impl std::ops::Neg for Vec2i
{
	type Output = Self;

	fn neg(self) -> Self::Output
	{
		Self::new(-self.file, -self.rank)
	}
}
impl std::ops::Add for Vec2i
{
	type Output = Self;

	fn add(self, rhs: Self) -> Self::Output
	{
		Self::new(self.file + rhs.file, self.rank - rhs.rank)
	}
}
impl std::ops::Sub for Vec2i
{
	type Output = Self;

	fn sub(self, rhs: Self) -> Self::Output
	{
		self + (-rhs)
	}
}
impl std::ops::Mul<i32> for Vec2i
{
	type Output = Self;

	fn mul(self, rhs: i32) -> Self::Output
	{
		Self::new(self.file * rhs, self.rank * rhs)
	}
}
impl std::ops::Mul<Vec2i> for i32
{
	type Output = Vec2i;

	fn mul(self, rhs: Vec2i) -> Self::Output
	{
		rhs * self
	}
}
impl Display for Vec2i
{
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
	{
		write!(f, "({}, {})", self.file, self.rank)
	}
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum Direction
{
	North,
	Northeast,
	East,
	Southeast,
	South,
	Southwest,
	West,
	Northwest,
}
impl Direction
{
	pub fn vector(self) -> Vec2i
	{
		// :( -morgan 2024-04-29
		use Direction::{East, North, Northeast, Northwest, South, Southeast, Southwest, West};
		match self
		{
			North => Vec2i::NORTH,
			Northeast => Vec2i::NORTH_EAST,
			East => Vec2i::EAST,
			Southeast => Vec2i::SOUTH_EAST,
			South => Vec2i::SOUTH,
			Southwest => Vec2i::SOUTH_WEST,
			West => Vec2i::WEST,
			Northwest => Vec2i::NORTH_WEST,
		}
	}

	pub fn axis(self) -> SlidingAxis
	{
		match self
		{
			Self::North | Self::East | Self::South | Self::West => SlidingAxis::Orthogonal,
			_ => SlidingAxis::Diagonal,
		}
	}
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
enum VerticalDirection
{
	Up,
	Down,
}
impl VerticalDirection
{
	fn from_i32(value: i32) -> Option<Self>
	{
		(value < 0)
			.then_some(Self::Down)
			.or_else(|| (value > 0).then_some(Self::Up))
	}
}

#[cfg(test)]
mod test
{
	use crate::board::{positioning::HorizontalDirection, Pos, Vec2i};

	#[test]
	fn pos_initialization()
	{
		assert_eq!(Pos::from_file_rank(4, 6), Pos::from_index(12));
		assert_eq!(Pos::from_file_rank(0, 0), Pos::from_index(56));
		assert_eq!(Pos::from_file_rank(0, 7), Pos::from_index(0));
	}

	#[test]
	fn pos_addition()
	{
		assert_eq!(
			Pos::from_file_rank(1, 5) + Vec2i::new(2, 1),
			Pos::from_file_rank(3, 6)
		);

		assert_eq!(
			Pos::from_file_rank(4, 2) + Vec2i::new(0, 0),
			Pos::from_file_rank(4, 2)
		);

		assert_eq!(
			Pos::from_file_rank(2, 6) + Vec2i::NORTH_EAST,
			Pos::from_file_rank(3, 7)
		);
	}

	#[test]
	fn offset_direction()
	{
		let pos_a = Pos::from_index(30);
		let pos_b = Pos::from_index(31);

		assert_eq!(
			pos_a.offset_to(pos_b).directions().0,
			Some(HorizontalDirection::Right)
		);
	}

	#[test]
	#[should_panic(expected = "Board position out of bounds!")]
	fn out_of_bounds_addition()
	{
		let _ = Pos::from_file_rank(7, 7) + Vec2i::new(2, 2);
	}
}
