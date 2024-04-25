use std::{borrow::BorrowMut, fmt::Display};

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

impl Board
{
	fn fen_regex() -> &'static Regex
	{
		lazy_regex::regex!(
			r"(?<pieces>(?:[pnrbkqPNRBKQ1-8]+/){7}[pnrbkqPNRBKQ1-8]+)\s+(?<to_move>[wb])\s+(?<castle>\-|[KQkq]+)\s(?<en_passant>\-|[a-h][36])\s(?<halfmove>\d+)\s(?<fullmove>\d+)"
		)
	}
	pub fn from_fen_string(fen: &str) -> Option<Self>
	{
		let captures = Self::fen_regex().captures(fen)?;

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

	pub fn legal_moves(&self) -> Vec<Move>
	{
		// wow i really hate this - morgan 2024-04-23
		let movable_pieces = self.pieces.iter().enumerate().filter_map(|it| {
			it.1.map_or(None, |piece| {
				if piece.color == self.to_move
				{
					Some((it.0, piece))
				}
				else
				{
					None
				}
			})
		});

		let mut legal_moves = Vec::new();
		for (index, piece) in movable_pieces
		{
			let current_pos = BoardPos::from_index(index);
			let valid_targets = match piece.piece_type
			{
				PieceType::Pawn =>
				{
					let direction = if piece.color == PieceColor::White
					{
						1
					}
					else
					{
						-1
					};

					let mut targets = Vec::from([
						current_pos + Vec2i::new(0, direction),
						current_pos + Vec2i::new(1, direction),
						current_pos + Vec2i::new(-1, direction),
					]);

					if current_pos.rank() == 1 || current_pos.rank() == 6
					{
						targets.push(current_pos + Vec2i::new(0, 2 * direction));
					}

					targets
				}
				PieceType::Knight => Vec::from([
					current_pos + Vec2i::new(2, 1),
					current_pos + Vec2i::new(2, -1),
					current_pos + Vec2i::new(-2, 1),
					current_pos + Vec2i::new(-2, -1),
					current_pos + Vec2i::new(1, 2),
					current_pos + Vec2i::new(1, -2),
					current_pos + Vec2i::new(-1, 2),
					current_pos + Vec2i::new(-1, -2),
				]),
				PieceType::King => Vec::from([
					current_pos + Vec2i::new(1, 0),
					current_pos + Vec2i::new(-1, 0),
					current_pos + Vec2i::new(0, 1),
					current_pos + Vec2i::new(0, -1),
					current_pos + Vec2i::new(1, 1),
					current_pos + Vec2i::new(1, -1),
					current_pos + Vec2i::new(-1, 1),
					current_pos + Vec2i::new(-1, -1),
				]),
				PieceType::Queen => self.sliding_piece_targets(current_pos, SlidingAxis::Both),
				PieceType::Bishop => self.sliding_piece_targets(current_pos, SlidingAxis::Diagonal),
				PieceType::Rook => self.sliding_piece_targets(current_pos, SlidingAxis::Orthogonal),
			};

			for target_pos in valid_targets.into_iter().flatten()
			{
				if self.can_piece_play(piece, Move::new(current_pos, target_pos))
				{
					legal_moves.push(Move::new(current_pos, target_pos));
				}
			}
		}

		legal_moves
	}

	pub fn make_move(&mut self, mov: Move)
	{
		self.pieces[mov.to.index()] = self.pieces[mov.from.index()].take();
		self.to_move = !self.to_move;
	}

	fn sliding_piece_targets(&self, from: BoardPos, axis: SlidingAxis) -> Vec<Option<BoardPos>>
	{
		let mut targets = Vec::new();
		let mut cursor = from;

		let mut valid_offsets = Vec::new();
		if axis.orthogonal_allowed()
		{
			valid_offsets.append(
				Vec::from([
					Vec2i::new(1, 0),
					Vec2i::new(-1, 0),
					Vec2i::new(0, 1),
					Vec2i::new(0, -1),
				])
				.borrow_mut(),
			);
		}
		if axis.diagonal_allowed()
		{
			valid_offsets.append(
				Vec::from([
					Vec2i::new(1, 1),
					Vec2i::new(1, -1),
					Vec2i::new(-1, 1),
					Vec2i::new(-1, -1),
				])
				.borrow_mut(),
			);
		}

		for offset in valid_offsets
		{
			while let Some(new_pos) = cursor + offset
			{
				targets.push(Some(new_pos));
				if self[new_pos].is_some()
				{
					break;
				}
				cursor = new_pos;
			}

			cursor = from;
		}
		targets
	}

	fn can_piece_play(&self, piece: Piece, mov: Move) -> bool
	{
		let can_move_to = piece.piece_type != PieceType::Pawn || mov.from.file() == mov.to.file();
		let can_capture = piece.piece_type != PieceType::Pawn || mov.from.file() != mov.to.file();
		let target_piece = self[mov.to];

		(can_move_to && target_piece.is_none())
			|| (target_piece.is_some_and(|t| can_capture && t.color != piece.color))
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
	pub fn new(file: i32, rank: i32) -> Self
	{
		Self { file, rank }
	}
}
impl std::ops::Neg for Vec2i
{
	type Output = Self;

	fn neg(self) -> Self::Output
	{
		Self {
			file: -self.file,
			rank: -self.rank,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Move
{
	pub from: BoardPos,
	pub to: BoardPos,
}
impl Move
{
	pub fn new(from: BoardPos, to: BoardPos) -> Self
	{
		Self { from, to }
	}
}
impl Display for Move
{
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
	{
		write!(f, "{} --> {}", self.from, self.to)
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
			tile_index: 8 * rank + file,
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

	pub fn move_by(self, offset: Vec2i) -> Option<BoardPos>
	{
		let (signed_rank, signed_file) = (self.top_down_rank() as i32, self.file() as i32);
		let unclamped_pos = ((signed_rank - offset.rank), (signed_file + offset.file));
		let new_pos = ((0..=7).contains(&unclamped_pos.0) && (0..=7).contains(&unclamped_pos.1))
			.then(|| {
				Self::from_rank_file(
					unclamped_pos.0.clamp(0, 7) as u8,
					unclamped_pos.1.clamp(0, 7) as u8,
				)
			});

		new_pos.and_then(|pos| (pos != self).then_some(pos))
	}
}
impl Display for BoardPos
{
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
	{
		write!(f, "{}{}", self.file_char(), self.rank() + 1)
	}
}
impl std::ops::Add<Vec2i> for BoardPos
{
	type Output = Option<Self>;

	fn add(self, rhs: Vec2i) -> Self::Output
	{
		self.move_by(rhs)
	}
}
impl std::ops::Sub<Vec2i> for BoardPos
{
	type Output = Option<Self>;

	fn sub(self, rhs: Vec2i) -> Self::Output
	{
		self.move_by(-rhs)
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
impl Display for PieceColor
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
