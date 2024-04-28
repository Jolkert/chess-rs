use std::{borrow::BorrowMut, fmt::Display};

use eframe::egui::Vec2;
use lazy_regex::Regex;

use crate::pieces::{Color, Piece, PieceType, SlidingAxis};

// TODO: check
#[derive(Debug)]
pub struct Board
{
	pieces: [Option<Piece>; 64],
	to_move: Color,
	en_passant_target: Option<Pos>,
	castle_legality: CastleLegality,
	pub legal_moves: Vec<Move>,
}
impl Default for Board
{
	fn default() -> Self
	{
		Self {
			pieces: [None; 64],
			to_move: Color::White,
			en_passant_target: None,
			castle_legality: CastleLegality::default(),
			legal_moves: Vec::new(),
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
impl std::ops::Index<Pos> for Board
{
	type Output = Option<Piece>;

	fn index(&self, index: Pos) -> &Self::Output
	{
		&self.pieces[index.index()]
	}
}
impl std::ops::IndexMut<Pos> for Board
{
	fn index_mut(&mut self, index: Pos) -> &mut Self::Output
	{
		&mut self.pieces[index.index()]
	}
}
impl Board
{
	// pub fn has_king_moved(&self) -> WhiteBlackPair<bool>
	// {
	// 	self.castle_legality.has_king_moved
	// }
	// pub fn has_a_rook_moved(&self) -> WhiteBlackPair<bool>
	// {
	// 	self.castle_legality.has_a_rook_moved
	// }
	// pub fn has_h_rook_moved(&self) -> WhiteBlackPair<bool>
	// {
	// 	self.castle_legality.has_h_rook_moved
	// }

	fn new(
		pieces: [Option<Piece>; 64],
		to_move: Color,
		en_passant_target: Option<Pos>,
		castle_legality: CastleLegality,
	) -> Self
	{
		let mut new_self = Self {
			pieces,
			to_move,
			en_passant_target,
			castle_legality,
			legal_moves: Vec::new(),
		};
		new_self.regen_legal_moves();
		new_self
	}

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
			'w' => Some(Color::White),
			'b' => Some(Color::Black),
			_ => None,
		}?;

		let castle_options = captures[3].chars().collect::<Vec<_>>();
		let white_kingside = castle_options.contains(&'K');
		let white_queenside = castle_options.contains(&'Q');
		let black_kingside = castle_options.contains(&'k');
		let black_queenside = castle_options.contains(&'q');

		let en_passant_str = &captures[4];
		let en_passant_target = (en_passant_str != "-").then(|| {
			let file = en_passant_str
				.chars()
				.nth(0)
				.map(|it| (it as u8) - 97)
				.unwrap();
			let rank = en_passant_str.chars().nth(1).unwrap().to_digit(10).unwrap();

			Pos::from_rank_file(8 - rank as u8, file)
		});

		Some(Self::new(
			pieces,
			to_move,
			en_passant_target,
			CastleLegality {
				white_kingside,
				white_queenside,
				black_kingside,
				black_queenside,
			},
		))
	}

	pub fn to_move(&self) -> Color
	{
		self.to_move
	}

	pub fn en_passant_target(&self) -> Option<Pos>
	{
		self.en_passant_target
	}

	pub fn castle_legality(&self) -> CastleLegality
	{
		self.castle_legality
	}

	fn regen_legal_moves(&mut self)
	{
		self.legal_moves = self.gen_legal_moves();
	}

	pub fn gen_legal_moves(&mut self) -> Vec<Move>
	{
		let pseudo_legal_moves = self.pseudo_legal_moves_for_color(self.to_move);
		pseudo_legal_moves
			.into_iter()
			.filter(|mov| {
				let check_blocks_move = self.move_endangers_king(*mov);
				let piece = self[mov.from].unwrap();
				let check_blocks_castle = if piece.is_king() && mov.offset().file.abs() > 1
				{
					self.check_blocks_castle(mov.from, piece.color, mov.offset().normalized())
				}
				else
				{
					false
				};

				!check_blocks_move && !check_blocks_castle
			})
			.collect()
	}

	pub fn pseudo_legal_moves_for_color(&self, color: Color) -> Vec<Move>
	{
		// do i hate this? -morgan 2024-04-27
		self.pieces
			.iter()
			.enumerate()
			.filter_map(|(i, square)| {
				square
					.and_then(|piece| (piece.color == color).then_some((Pos::from_index(i), piece)))
			})
			.flat_map(|(pos, piece)| self.pseudo_legal_moves_for_piece(pos, piece))
			.collect::<Vec<_>>()
	}

	pub fn pseudo_legal_moves_for_piece(&self, position: Pos, piece: Piece) -> Vec<Move>
	{
		match piece.piece_type
		{
			PieceType::Pawn =>
			{
				let forward = piece.forward_vector();
				Vec::from([
					position + forward,
					position + forward + Vec2i::RIGHT,
					position + forward + Vec2i::LEFT,
					((position.rank() == 1 || position.rank() == 6)
						&& (position + forward).is_some_and(|sq| self[sq].is_none()))
					.then(|| position + (forward * 2))
					.flatten(),
				])
			}
			PieceType::Knight => Vec::from([
				position + Vec2i::new(2, 1),
				position + Vec2i::new(2, -1),
				position + Vec2i::new(-2, 1),
				position + Vec2i::new(-2, -1),
				position + Vec2i::new(1, 2),
				position + Vec2i::new(1, -2),
				position + Vec2i::new(-1, 2),
				position + Vec2i::new(-1, -2),
			]),
			PieceType::King => Vec::from([
				position + Vec2i::LEFT,
				position + Vec2i::UP,
				position + Vec2i::RIGHT,
				position + Vec2i::DOWN,
				position + Vec2i::new(1, 1),
				position + Vec2i::new(1, -1),
				position + Vec2i::new(-1, 1),
				position + Vec2i::new(-1, -1),
				position + Vec2i::new(2, 0),
				position + Vec2i::new(-2, 0),
			]),
			PieceType::Queen => self.sliding_piece_targets(position, SlidingAxis::Both),
			PieceType::Bishop => self.sliding_piece_targets(position, SlidingAxis::Diagonal),
			PieceType::Rook => self.sliding_piece_targets(position, SlidingAxis::Orthogonal),
		}
		// is this a good idea? -morgan 2024-04-27
		.into_iter()
		.flatten()
		.map(|target_pos| Move::new(position, target_pos))
		.filter(|mov| self.can_piece_play(piece, *mov))
		.collect()
	}

	pub fn make_move(&mut self, mov: Move) -> PlayedMove
	{
		let result = self.make_move_no_regen(mov);
		self.regen_legal_moves();
		result
	}
	pub fn unmake_move(&mut self, mov: &PlayedMove)
	{
		self.unmake_move_no_regen(mov);
		self.regen_legal_moves();
	}

	fn make_move_no_regen(&mut self, mov: Move) -> PlayedMove
	{
		let piece = self[mov.from].expect("Tried to make a move from an empty square!");
		let previous_castle_legality = self.castle_legality;
		let previous_en_passant = self.en_passant_target;

		let is_en_passant =
			piece.is_pawn() && self[mov.to].is_none() && mov.from.file() != mov.to.file();
		let is_castle = piece.is_king() && mov.offset().file.abs() > 1;

		let capture_square = is_en_passant
			.then(|| (mov.to - piece.forward_vector()).expect("En passant broke"))
			.unwrap_or(mov.to);

		let capture_data = self[capture_square].map(|piece| Capture {
			pos: capture_square,
			piece,
		});

		self[mov.to] = self[mov.from].take();
		if is_en_passant
		{
			self[(mov.to - piece.forward_vector()).expect("En passant broke")] = None;
		}

		let castle_state = is_castle.then(|| {
			let direction = mov.offset().normalized();
			let (rook_position, state) = if direction == Vec2i::LEFT
			{
				(mov.from.to_left_edge(), CastleSide::Queenside)
			}
			else
			{
				(mov.from.to_right_edge(), CastleSide::Kingside)
			};

			self[(mov.from + direction).unwrap()] = self[rook_position].take();

			state
		});

		if piece.is_king()
		{
			self.castle_legality.disallow_for_color(piece.color);
		}
		if piece.piece_type == PieceType::Rook
			&& let Some(castle_side) = (mov.from.file() == 0)
				.then_some(CastleSide::Queenside)
				.or_else(|| (mov.from.file() == 7).then_some(CastleSide::Kingside))
		{
			self.castle_legality.disallow(piece.color, castle_side);
		}

		self.en_passant_target = (piece.is_pawn() && mov.from.rank().abs_diff(mov.to.rank()) > 1)
			.then(|| mov.to - piece.forward_vector())
			.flatten();
		self.to_move = !self.to_move;

		let check_state = if self.pseudo_legal_moves_for_color(!piece.color).is_empty()
		{
			CheckState::Checkmate
		}
		else if self.is_king_in_check(!piece.color)
		{
			CheckState::Check
		}
		else
		{
			CheckState::None
		};

		PlayedMove {
			mov,
			piece,
			capture: capture_data,
			castle_state,
			check_state,
			previous_castle_legality,
			previous_en_passant,
		}
	}

	fn unmake_move_no_regen(&mut self, mov: &PlayedMove)
	{
		let _ = self[mov.from()].insert(mov.piece);
		self[mov.to()] = None;
		if let Some(capture) = mov.capture
		{
			let _ = self[capture.pos].insert(capture.piece);
		}

		if let Some(castle_state) = mov.castle_state
		{
			let rook_start_pos = match castle_state
			{
				CastleSide::Kingside => mov.to().to_right_edge(),
				CastleSide::Queenside => mov.to().to_left_edge(),
			};

			let _ = self[rook_start_pos].insert(Piece::new(mov.piece.color, PieceType::Rook));
			let rook_pos_after_castle = (mov.from() + castle_state.direction())
				.expect("Attempted to undo invalid castle move!");
			self[rook_pos_after_castle] = None;
		}

		self.to_move = mov.piece.color;
		self.castle_legality = mov.previous_castle_legality;
		self.en_passant_target = mov.previous_en_passant;
	}

	fn sliding_piece_targets(&self, from: Pos, axis: SlidingAxis) -> Vec<Option<Pos>>
	{
		let mut targets = Vec::new();
		let mut cursor = from;

		let mut valid_offsets = Vec::new();
		if axis.orthogonal_allowed()
		{
			valid_offsets.append(
				Vec::from([Vec2i::LEFT, Vec2i::UP, Vec2i::RIGHT, Vec2i::DOWN]).borrow_mut(),
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
		let can_move_to = !piece.is_pawn() || mov.from.file() == mov.to.file();
		let can_capture = !piece.is_pawn() || mov.from.file() != mov.to.file();
		let is_castle = piece.is_king() && mov.offset().file.abs() > 1;
		let target_piece = self[mov.to];

		let move_allowed = can_move_to && target_piece.is_none() && !is_castle;
		let capture_allowed = can_capture
			&& !is_castle
			&& (target_piece.is_some_and(|p| p.color != piece.color)
				|| self.en_passant_target.is_some_and(|sq| sq == mov.to));

		// this castle detection is a nightmare -morgan 2024-04-26
		// its still not pretty, but i think its a bit better now?
		// could do with more function outlining -morgan 2024-04-28
		let castle_allowed = piece.is_king() && {
			let move_direction = mov.offset().normalized();
			let rook_pos = mov.from.to_edge_in_direction_of(mov.offset());
			let side_allows_castles = match rook_pos.file()
			{
				0 => self
					.castle_legality
					.is_allowed(piece.color, CastleSide::Queenside),
				7 => self
					.castle_legality
					.is_allowed(piece.color, CastleSide::Kingside),
				_ => false,
			};

			let result = self[rook_pos].is_some_and(|rook| {
				rook.piece_type == PieceType::Rook
					&& side_allows_castles
					&& self
						.pseudo_legal_moves_for_piece(rook_pos, rook)
						.iter()
						.any(|it| it.to == (mov.from + move_direction).unwrap())
			});
			result
		};

		move_allowed || capture_allowed || castle_allowed
	}

	fn move_endangers_king(&mut self, mov: Move) -> bool
	{
		let played_move = self.make_move_no_regen(mov);
		let result = self.is_king_in_check(!self.to_move);
		self.unmake_move_no_regen(&played_move);
		result
	}

	fn check_blocks_castle(&self, start_pos: Pos, color: Color, direction: Vec2i) -> bool
	{
		let mut castle_passing_squares = [start_pos + direction, start_pos + (direction * 2)]
			.into_iter()
			.flatten();

		self.is_king_in_check(color)
			|| castle_passing_squares.any(|passing_square| {
				self.pseudo_legal_moves_for_color(!color)
					.iter()
					.any(|mov| mov.to == passing_square)
			})
	}

	fn is_king_in_check(&self, color: Color) -> bool
	{
		self.pseudo_legal_moves_for_color(!color)
			.iter()
			.any(|mov| mov.to == self.king_postion(color))
	}

	fn king_postion(&self, color: Color) -> Pos
	{
		Pos::from_index(
			self.pieces
				.iter()
				.enumerate()
				.find(|(_, square)| {
					square.is_some_and(|piece| piece.color == color && piece.is_king())
				})
				.unwrap_or_else(|| panic!("{color} king not found!"))
				.0,
		)
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Move
{
	pub from: Pos,
	pub to: Pos,
}
impl Move
{
	pub fn new(from: Pos, to: Pos) -> Self
	{
		Self { from, to }
	}

	pub fn offset(self) -> Vec2i
	{
		self.to.offset_from(self.from)
	}
}
impl Display for Move
{
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
	{
		write!(f, "{} --> {}", self.from, self.to)
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PlayedMove
{
	mov: Move,
	piece: Piece,
	capture: Option<Capture>,
	castle_state: Option<CastleSide>,
	check_state: CheckState,
	previous_castle_legality: CastleLegality,
	previous_en_passant: Option<Pos>,
}
impl Display for PlayedMove
{
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
	{
		match self.castle_state
		{
			None => write!(
				f,
				"{}{}{}",
				self.piece.piece_type,
				self.capture.is_some().then_some("x").unwrap_or_default(),
				self.mov.to
			),
			Some(CastleSide::Kingside) => write!(f, "O-O"),
			Some(CastleSide::Queenside) => write!(f, "O-O-O"),
		}
	}
}
impl PlayedMove
{
	pub fn from(&self) -> Pos
	{
		self.mov.from
	}
	pub fn to(&self) -> Pos
	{
		self.mov.to
	}

	pub fn check_state(&self) -> CheckState
	{
		self.check_state
	}
	pub fn piece(&self) -> Piece
	{
		self.piece
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Capture
{
	pub pos: Pos,
	pub piece: Piece,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CheckState
{
	None,
	Check,
	Checkmate,
}
impl Display for CheckState
{
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
	{
		write!(
			f,
			"{}",
			match self
			{
				Self::None => "",
				Self::Check => "+",
				Self::Checkmate => "#",
			}
		)
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CastleSide
{
	Kingside,
	Queenside,
}
impl CastleSide
{
	pub fn direction(self) -> Vec2i
	{
		match self
		{
			Self::Kingside => Vec2i::RIGHT,
			Self::Queenside => Vec2i::LEFT,
		}
	}
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
// these are all independently variable from one another
// and i dont see the need to make the type a separate enum
// -morgan 2024-04-28
#[allow(clippy::struct_excessive_bools)]
pub struct CastleLegality
{
	white_kingside: bool,
	white_queenside: bool,
	black_kingside: bool,
	black_queenside: bool,
}
impl CastleLegality
{
	pub fn is_allowed(self, color: Color, side: CastleSide) -> bool
	{
		match (color, side)
		{
			(Color::White, CastleSide::Kingside) => self.white_kingside,
			(Color::White, CastleSide::Queenside) => self.white_queenside,
			(Color::Black, CastleSide::Kingside) => self.black_kingside,
			(Color::Black, CastleSide::Queenside) => self.black_queenside,
		}
	}

	pub fn disallow(&mut self, color: Color, side: CastleSide)
	{
		*self.value_of(color, side) = false;
	}

	pub fn disallow_for_color(&mut self, color: Color)
	{
		*self.value_of(color, CastleSide::Kingside) = false;
		*self.value_of(color, CastleSide::Queenside) = false;
	}

	fn value_of(&mut self, color: Color, side: CastleSide) -> &mut bool
	{
		match (color, side)
		{
			(Color::White, CastleSide::Kingside) => &mut self.white_kingside,
			(Color::White, CastleSide::Queenside) => &mut self.white_queenside,
			(Color::Black, CastleSide::Kingside) => &mut self.black_kingside,
			(Color::Black, CastleSide::Queenside) => &mut self.black_queenside,
		}
	}
}

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
	pub const fn from_rank_file(rank: u8, file: u8) -> Self
	{
		Self::from_top_down_rank_file(7 - rank, file)
	}
	pub const fn from_top_down_rank_file(rank: u8, file: u8) -> Self
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
		char::from_u32(u32::from(self.file()) + 97).expect("Invalid file character!")
	}

	pub fn move_by(self, offset: Vec2i) -> Option<Self>
	{
		let (signed_rank, signed_file) = (i32::from(self.rank()), i32::from(self.file()));
		let unclamped_pos = ((signed_rank + offset.rank), (signed_file + offset.file));
		let new_pos = ((0..=7).contains(&unclamped_pos.0) && (0..=7).contains(&unclamped_pos.1))
			.then(|| {
				Self::from_rank_file(
					unclamped_pos.0.clamp(0, 7) as u8,
					unclamped_pos.1.clamp(0, 7) as u8,
				)
			});

		new_pos.and_then(|pos| (pos != self).then_some(pos))
	}

	pub fn offset_from(self, other: Self) -> Vec2i
	{
		Vec2i::new(
			i32::from(self.file()) - i32::from(other.file()),
			i32::from(self.rank()) - i32::from(other.rank()),
		)
	}

	pub fn to_left_edge(self) -> Self
	{
		Self::from_rank_file(self.rank(), 0)
	}
	pub fn to_right_edge(self) -> Self
	{
		Self::from_rank_file(self.rank(), 7)
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

		Self::from_rank_file(rank, file)
	}
}
impl Display for Pos
{
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
	{
		write!(f, "{}{}", self.file_char(), self.rank() + 1)
	}
}
impl std::ops::Add<Vec2i> for Pos
{
	type Output = Option<Self>;

	fn add(self, rhs: Vec2i) -> Self::Output
	{
		self.move_by(rhs)
	}
}
impl std::ops::Add<Vec2i> for Option<Pos>
{
	type Output = Self;

	fn add(self, rhs: Vec2i) -> Self::Output
	{
		self.and_then(|lhs| lhs + rhs)
	}
}
impl std::ops::Sub<Vec2i> for Pos
{
	type Output = Option<Self>;

	fn sub(self, rhs: Vec2i) -> Self::Output
	{
		self.move_by(-rhs)
	}
}
impl std::ops::Sub<Vec2i> for Option<Pos>
{
	type Output = Self;

	fn sub(self, rhs: Vec2i) -> Self::Output
	{
		self.and_then(|lhs| lhs - rhs)
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Vec2i
{
	pub file: i32,
	pub rank: i32,
}
impl Vec2i
{
	pub const LEFT: Self = Self::new(-1, 0);
	pub const UP: Self = Self::new(0, 1);
	pub const RIGHT: Self = Self::new(1, 0);
	pub const DOWN: Self = Self::new(0, -1);

	pub const fn new(file: i32, rank: i32) -> Self
	{
		Self { file, rank }
	}

	pub fn normalized(self) -> Self
	{
		Self::new(
			self.file.checked_div(self.file.abs()).unwrap_or_default(),
			self.rank.checked_div(self.rank.abs()).unwrap_or_default(),
		)
	}

	fn directions(self) -> (Option<HorizontalDirection>, Option<VerticalDirection>)
	{
		let normalized = self.normalized();
		(
			HorizontalDirection::from_i32(normalized.file),
			VerticalDirection::from_i32(normalized.rank),
		)
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
impl std::ops::Mul<i32> for Vec2i
{
	type Output = Self;

	fn mul(self, rhs: i32) -> Self::Output
	{
		Self::new(self.file * rhs, self.rank * rhs)
	}
}
impl Display for Vec2i
{
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
	{
		write!(f, "({}, {})", self.file, self.rank)
	}
}
