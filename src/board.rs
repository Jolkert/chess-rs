pub mod moves;
pub mod pieces;
mod positioning;

use std::{collections::VecDeque, iter};

pub use positioning::*;

use lazy_regex::Regex;

use self::{
	moves::{Capture, CastleLegality, CastleSide, CheckState, Move, PlayedMove},
	pieces::{Color, Piece, PieceType, PositionedPiece, SlidingAxis},
};

// TODO: check
#[derive(Debug)]
pub struct Board
{
	squares: [Option<Piece>; 64],
	to_move: Color,
	en_passant_target: Option<Pos>,
	castle_legality: CastleLegality,
	legal_moves: Vec<Move>,
	previous_moves: VecDeque<PlayedMove>,
}
impl Default for Board
{
	fn default() -> Self
	{
		Self {
			squares: [None; 64],
			to_move: Color::White,
			en_passant_target: None,
			castle_legality: CastleLegality::default(),
			legal_moves: Vec::new(),
			previous_moves: VecDeque::new(),
		}
	}
}
impl std::ops::Index<usize> for Board
{
	type Output = Option<Piece>;

	fn index(&self, index: usize) -> &Self::Output
	{
		&self.squares[index]
	}
}
impl std::ops::IndexMut<usize> for Board
{
	fn index_mut(&mut self, index: usize) -> &mut Self::Output
	{
		&mut self.squares[index]
	}
}
impl std::ops::Index<Pos> for Board
{
	type Output = Option<Piece>;

	fn index(&self, index: Pos) -> &Self::Output
	{
		&self.squares[index.index()]
	}
}
impl std::ops::IndexMut<Pos> for Board
{
	fn index_mut(&mut self, index: Pos) -> &mut Self::Output
	{
		&mut self.squares[index.index()]
	}
}
impl Board
{
	// conversion
	fn new(
		pieces: [Option<Piece>; 64],
		to_move: Color,
		en_passant_target: Option<Pos>,
		castle_legality: CastleLegality,
	) -> Self
	{
		let mut board = Self {
			squares: pieces,
			to_move,
			en_passant_target,
			castle_legality,
			..Default::default()
		};
		board.regen_legal_moves();
		board
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

			Pos::from_file_rank(8 - rank as u8, file)
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

	// accessors
	pub fn pieces(&self) -> impl Iterator<Item = &Piece>
	{
		self.squares.iter().flatten()
	}

	pub fn peices_of(&self, color: Color) -> impl Iterator<Item = &Piece> + '_
	{
		self.pieces().filter(move |it| it.color == color)
	}

	pub fn positioned_pieces(&self) -> impl Iterator<Item = PositionedPiece> + '_
	{
		self.squares.iter().enumerate().filter_map(|(i, square)| {
			square.map(|piece| PositionedPiece::new(Pos::from_index(i), piece))
		})
	}
	pub fn positioned_pieces_of(&self, color: Color) -> impl Iterator<Item = PositionedPiece> + '_
	{
		self.positioned_pieces()
			.filter(move |piece| piece.color() == color)
	}

	pub fn en_passant_target(&self) -> Option<Pos>
	{
		self.en_passant_target
	}

	pub fn castle_legality(&self) -> CastleLegality
	{
		self.castle_legality
	}

	pub fn legal_moves(&self) -> &[Move]
	{
		&self.legal_moves
	}

	fn regen_legal_moves(&mut self)
	{
		self.legal_moves = self.gen_legal_moves();
	}

	pub fn gen_legal_moves(&mut self) -> Vec<Move>
	{
		let mut pseudo_legal_moves = self.pseudo_legal_moves_for_color(self.to_move);
		pseudo_legal_moves.retain(|mov| self.is_move_legal(*mov));
		pseudo_legal_moves
	}

	pub fn is_move_castle(&self, mov: Move) -> bool
	{
		mov.offset().file.abs() > 1 && self[mov.from].is_some_and(Piece::is_king)
	}

	pub fn is_move_legal(&self, mov: Move) -> bool
	{
		let Some(moving_piece) = self[mov.from]
		else
		{
			return false;
		};

		if moving_piece.is_king()
		{
			let mut targeted_squares = self
				.positioned_pieces_of(!moving_piece.color)
				.flat_map(|piece| self.piece_targeted_squares(piece));

			if self.is_move_castle(mov)
			{
				let move_direction = mov.offset().normalized();
				targeted_squares
					.any(|sq| sq == mov.from || sq == mov.from + move_direction || sq == mov.to)
			}
			else
			{
				targeted_squares.all(|square| mov.to != square)
			}
		}
		else
		{
			let king_pos = self.king_pos_of(moving_piece.color);
			let all_attackers = self.pieces_attacking_king(moving_piece.color);

			all_attackers.len() < 2 && {
				let en_passant_vacant_square = self
					.is_move_en_passant(mov)
					.then(|| mov.from - moving_piece.forward_vector());

				let king_to_move_start = king_pos.offset_to(mov.from);

				// god this hurts my eyes -morgan 2024-05-04
				let reveals_check = king_to_move_start
					.is_straight_line()
					.then(|| king_to_move_start.normalized().compass_direction())
					.map(|direction| SlidingRay::new(king_pos, direction))
					.is_some_and(|ray| {
						self.first_hit_ignoring(ray, [Some(mov.from), en_passant_vacant_square])
							.is_some_and(|hit_piece| {
								hit_piece.can_slide_in_direction(ray.direction)
									&& hit_piece.color() != moving_piece.color
							})
					});

				all_attackers.first().map_or(!reveals_check, |attacker| {
					!reveals_check
						&& (self.capture_square(mov) == attacker.pos() || {
							let check_ray = attacker.sliding_ray(
								attacker
									.pos()
									.offset_to(king_pos)
									.normalized()
									.compass_direction(),
							);

							!check_ray.is_some_and(|ray| {
								self.first_hit_ignoring(ray, [Some(mov.from)]).is_some_and(
									|hit_piece| {
										hit_piece.can_slide_in_direction(ray.direction)
											&& hit_piece.color() != moving_piece.color
									},
								)
							})
						})
				})
			}
		}
	}

	fn is_move_en_passant(&self, mov: Move) -> bool
	{
		let result = self[mov.from].is_some_and(|piece| {
			piece.is_pawn()
				&& mov.to.file() != mov.from.file()
				&& self
					.en_passant_target
					.is_some_and(|ep_square| ep_square == mov.to)
		});

		if result
		{
			log::info!("Determined {mov} to be en_passant!");
		}

		result
	}

	/// Doesn't check that move is a capture, simply assumes it is
	fn capture_square(&self, mov: Move) -> Pos
	{
		(!self.is_move_en_passant(mov))
			.then_some(mov.to)
			.unwrap_or_else(|| mov.to - self[mov.from].unwrap().forward_vector())
	}

	fn pieces_attacking_king(&self, color: Color) -> Vec<PositionedPiece>
	{
		let Some(last_move) = self.previous_moves.front()
		else
		{
			return Vec::new();
		};

		let king_pos = self.king_pos_of(color);

		let direct_attacker = self[last_move.to()].and_then(|attacker| {
			let positioned_attacker = PositionedPiece::new(last_move.to(), attacker);
			self.piece_targeted_squares(positioned_attacker)
				.contains(&king_pos)
				.then_some(positioned_attacker)
		});

		let discovered_attackers = last_move.revealed_squares().filter_map(|square| {
			let offset_from_king = square.offset_from(king_pos);
			offset_from_king
				.is_straight_line()
				.then(|| {
					let direction_from_king = offset_from_king.normalized().compass_direction();
					let potential_attacker =
						self.first_hit(SlidingRay::new(king_pos, direction_from_king));

					potential_attacker.and_then(|piece| {
						piece
							.can_slide_in_direction(direction_from_king)
							.then_some(piece)
					})
				})
				.flatten()
		});

		iter::once(direct_attacker)
			.flatten()
			.chain(discovered_attackers)
			.collect()
	}

	fn first_hit(&self, ray: SlidingRay) -> Option<PositionedPiece>
	{
		ray.iter()
			.find_map(|pos| self[pos].map(|piece| PositionedPiece::new(pos, piece)))
	}

	fn first_hit_ignoring<const N: usize>(
		&self,
		ray: SlidingRay,
		ignore: [Option<Pos>; N],
	) -> Option<PositionedPiece>
	{
		ray.iter().find_map(|pos| {
			self[pos].and_then(|piece| {
				(!ignore.iter().flatten().any(|ignored| pos == *ignored))
					.then(|| PositionedPiece::new(pos, piece))
			})
		})
	}

	fn ray_squares_until_piece(&self, ray: SlidingRay) -> Vec<Pos>
	{
		// pls let me take the boundary element with take_while i beg you -morgan 2024-05-03
		let mut squares = Vec::new();
		for pos in ray.iter()
		{
			squares.push(pos);
			if self[pos].is_some()
			{
				break;
			}
		}

		squares
	}

	pub fn piece_targeted_squares(&self, piece: PositionedPiece) -> Vec<Pos>
	{
		let PositionedPiece(pos, piece) = piece;
		match piece.piece_type
		{
			PieceType::Pawn =>
			{
				let forward = piece.forward_vector();
				Vec::from([
					pos.checked_move(forward + Vec2i::EAST),
					pos.checked_move(forward + Vec2i::WEST),
				])
			}
			PieceType::Knight => Vec::from([
				pos.checked_move(Vec2i::new(2, 1)),
				pos.checked_move(Vec2i::new(2, -1)),
				pos.checked_move(Vec2i::new(-2, 1)),
				pos.checked_move(Vec2i::new(-2, -1)),
				pos.checked_move(Vec2i::new(1, 2)),
				pos.checked_move(Vec2i::new(1, -2)),
				pos.checked_move(Vec2i::new(-1, 2)),
				pos.checked_move(Vec2i::new(-1, -2)),
			]),
			PieceType::King => Vec::from([
				pos.checked_move(Vec2i::WEST),
				pos.checked_move(Vec2i::NORTH),
				pos.checked_move(Vec2i::EAST),
				pos.checked_move(Vec2i::SOUTH),
				pos.checked_move(Vec2i::new(1, 1)),
				pos.checked_move(Vec2i::new(1, -1)),
				pos.checked_move(Vec2i::new(-1, 1)),
				pos.checked_move(Vec2i::new(-1, -1)),
				pos.checked_move(Vec2i::new(2, 0)),
				pos.checked_move(Vec2i::new(-2, 0)),
			]),

			_ => piece
				.sliding_axis()
				.expect("Attempted to get sliding axis of non-sliding piece!")
				.allowed_directions()
				.flat_map(|direction| {
					self.ray_squares_until_piece(SlidingRay::new(pos, *direction))
						.into_iter()
						.map(Some)
				})
				.collect::<Vec<_>>(),
		}
		.into_iter()
		.flatten()
		.collect()
	}

	pub fn pseudo_legal_moves_for_color(&self, color: Color) -> Vec<Move>
	{
		// do i hate this? -morgan 2024-04-27
		self.squares
			.iter()
			.enumerate()
			.filter_map(|(i, square)| {
				square.and_then(|piece| {
					(piece.color == color)
						.then_some(PositionedPiece::new(Pos::from_index(i), piece))
				})
			})
			.flat_map(|piece| self.pseudo_legal_moves_for_piece(piece))
			.collect::<Vec<_>>()
	}

	pub fn pseudo_legal_moves_for_piece(&self, piece: PositionedPiece) -> Vec<Move>
	{
		let PositionedPiece(pos, piece) = piece;
		match piece.piece_type
		{
			PieceType::Pawn =>
			{
				let forward = piece.forward_vector();
				Vec::from([
					pos.checked_move(forward),
					pos.checked_move(forward + Vec2i::EAST),
					pos.checked_move(forward + Vec2i::WEST),
					((pos.rank() == 1 || pos.rank() == 6)
						&& pos
							.checked_move(forward)
							.is_some_and(|sq| self[sq].is_none()))
					.then(|| pos.checked_move(2 * forward))
					.flatten(),
				])
			}
			PieceType::Knight => Vec::from([
				pos.checked_move(Vec2i::new(2, 1)),
				pos.checked_move(Vec2i::new(2, -1)),
				pos.checked_move(Vec2i::new(-2, 1)),
				pos.checked_move(Vec2i::new(-2, -1)),
				pos.checked_move(Vec2i::new(1, 2)),
				pos.checked_move(Vec2i::new(1, -2)),
				pos.checked_move(Vec2i::new(-1, 2)),
				pos.checked_move(Vec2i::new(-1, -2)),
			]),
			PieceType::King => Vec::from([
				pos.checked_move(Vec2i::WEST),
				pos.checked_move(Vec2i::NORTH),
				pos.checked_move(Vec2i::EAST),
				pos.checked_move(Vec2i::SOUTH),
				pos.checked_move(Vec2i::new(1, 1)),
				pos.checked_move(Vec2i::new(1, -1)),
				pos.checked_move(Vec2i::new(-1, 1)),
				pos.checked_move(Vec2i::new(-1, -1)),
				pos.checked_move(Vec2i::new(2, 0)),
				pos.checked_move(Vec2i::new(-2, 0)),
			]),
			_ => self.sliding_piece_targets(
				pos,
				piece
					.sliding_axis()
					.expect("Attempted to get sliding axis from non-sliding piece!"),
			),
		}
		// is this a good idea? -morgan 2024-04-27
		.into_iter()
		.flatten()
		.map(|target_pos| Move::new(pos, target_pos))
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
			.then(|| mov.to - piece.forward_vector())
			.unwrap_or(mov.to);

		let capture_data = self[capture_square].map(|piece| Capture {
			pos: capture_square,
			piece,
		});

		self[mov.to] = self[mov.from].take();
		if is_en_passant
		{
			self[mov.to - piece.forward_vector()] = None;
		}

		let castle_state = is_castle.then(|| {
			let direction = mov.offset().normalized();
			let (rook_position, state) = if direction == Vec2i::WEST
			{
				(mov.from.to_left_edge(), CastleSide::Queenside)
			}
			else
			{
				(mov.from.to_right_edge(), CastleSide::Kingside)
			};

			self[mov.from + direction] = self[rook_position].take();

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
			.then(|| mov.to - piece.forward_vector());
		self.to_move = !self.to_move;

		let check_state = CheckState::None; // if self.pseudo_legal_moves_for_color(!piece.color).is_empty()
									// {
									// 	CheckState::Checkmate
									// }
									// else if self.is_king_in_check(!piece.color)
									// {
									// 	CheckState::Check
									// }
									// else
									// {
									// 	CheckState::None
									// };

		let played = PlayedMove {
			mov,
			piece,
			capture: capture_data,
			castle_state,
			check_state,
			previous_castle_legality,
			previous_en_passant,
		};

		self.previous_moves.push_front(played.clone());
		played
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
			let rook_pos_after_castle = mov.from() + castle_state.direction();
			self[rook_pos_after_castle] = None;
		}

		self.to_move = mov.piece.color;
		self.castle_legality = mov.previous_castle_legality;
		self.en_passant_target = mov.previous_en_passant;
	}

	fn sliding_piece_targets(&self, pos: Pos, axis: SlidingAxis) -> Vec<Option<Pos>>
	{
		let mut targets = Vec::new();

		for ray in axis
			.allowed_directions()
			.map(|dir| SlidingRayIter::new(pos, *dir))
		{
			for square in ray
			{
				targets.push(Some(square));
				if self[square].is_some()
				{
					break;
				}
			}
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
			let rook_start_pos = mov.from.to_edge_in_direction_of(mov.offset());
			let rook_end_pos = mov.from + move_direction;
			let side_allows_castles = match rook_start_pos.file()
			{
				0 => self
					.castle_legality
					.is_allowed(piece.color, CastleSide::Queenside),
				7 => self
					.castle_legality
					.is_allowed(piece.color, CastleSide::Kingside),
				_ => false,
			};

			let result = self[rook_end_pos].is_none()
				&& self[rook_start_pos].is_some_and(|rook| {
					rook.piece_type == PieceType::Rook
						&& side_allows_castles && self
						.pseudo_legal_moves_for_piece(PositionedPiece::new(rook_start_pos, rook))
						.iter()
						.any(|it| it.to == mov.from + move_direction)
				});
			result
		};

		move_allowed || capture_allowed || castle_allowed
	}

	fn king_pos_of(&self, color: Color) -> Pos
	{
		self.squares
			.iter()
			.position(|square| square.is_some_and(|piece| piece.is_king() && piece.color == color))
			.map_or_else(|| panic!("Could not find {color} king!"), Pos::from_index)
	}
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct SlidingRay
{
	start_pos: Pos,
	direction: Direction,
}
impl SlidingRay
{
	pub fn new(start_pos: Pos, direction: Direction) -> Self
	{
		Self {
			start_pos,
			direction,
		}
	}

	pub fn intersects(self, target: Pos) -> bool
	{
		self.iter().any(|it| it == target)
	}

	pub fn first_hit(self, a: Pos, b: Pos) -> Option<Pos>
	{
		self.iter().find(|pos| *pos == a || *pos == b)
	}

	pub fn iter(self) -> impl Iterator<Item = Pos>
	{
		SlidingRayIter::new(self.start_pos, self.direction)
	}
}

pub struct SlidingRayIter
{
	pos: Pos,
	direction: Vec2i,
}
impl SlidingRayIter
{
	pub fn new(start_pos: Pos, direction: Direction) -> Self
	{
		Self {
			pos: start_pos,
			direction: direction.vector(),
		}
	}
}
impl Iterator for SlidingRayIter
{
	type Item = Pos;

	fn next(&mut self) -> Option<Self::Item>
	{
		let new_pos = self.pos.checked_move(self.direction);
		if let Some(new_pos) = new_pos
		{
			self.pos = new_pos;
		}
		new_pos
	}
}

#[cfg(test)]
#[allow(dead_code)]
mod test
{
	use crate::board::pieces::PositionedPiece;

	use super::{pieces::Piece, Board, Direction, Pos, SlidingRay, SlidingRayIter};

	#[test]
	fn ray_test()
	{
		let start_pos = Pos::from_file_rank(3, 4);

		let east = SlidingRayIter::new(start_pos, Direction::East)
			.map(Pos::index)
			.collect::<Vec<_>>();
		assert_eq!(east, [28, 29, 30, 31]);

		let north_east = SlidingRayIter::new(start_pos, Direction::Northeast)
			.map(Pos::index)
			.collect::<Vec<_>>();
		assert_eq!(north_east, [20, 13, 6]);

		let north_east = SlidingRayIter::new(start_pos, Direction::Northeast)
			.map(Pos::index)
			.collect::<Vec<_>>();
		assert_eq!(north_east, [20, 13, 6]);

		let north_west = SlidingRayIter::new(start_pos, Direction::Northwest)
			.map(Pos::index)
			.collect::<Vec<_>>();
		assert_eq!(north_west, [18, 9, 0]);
	}

	#[test]
	fn target_square_test()
	{
		let board = Board::from_fen_string("8/1p2k3/8/3r4/8/7P/6B1/3K4 w - - 0 1")
			.expect("Invalid FEN given to test!");

		let mut targets = board.piece_targeted_squares(PositionedPiece::new(
			Pos::from_file_rank(6, 1),
			Piece::from_char('B').expect("Invalid piece character!"),
		));

		targets.sort_by_key(|a| a.index());
		assert_eq!(
			targets.into_iter().map(Pos::index).collect::<Vec<_>>(),
			[27, 36, 45, 47, 61, 63]
		);
	}

	#[test]
	fn ray_intersection_test()
	{
		let ray = SlidingRay::new(Pos::from_file_rank(3, 3), Direction::Northeast);
		assert_eq!(
			ray.first_hit(Pos::from_index(21), Pos::from_index(14)),
			Some(Pos::from_index(21))
		);

		assert_eq!(ray.first_hit(Pos::from_index(5), Pos::from_index(6)), None);
	}

	#[test]
	fn move_generation_accuracy()
	{
		let mut starting_position_board =
			Board::from_fen_string("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1 ")
				.expect("Invalid FEN string fed to test!");

		assert_eq!(starting_position_board.perft(1), 20);
		assert_eq!(starting_position_board.perft(2), 400);
		assert_eq!(starting_position_board.perft(3), 8902);
		assert_eq!(starting_position_board.perft(4), 197_281);
	}

	impl Board
	{
		fn perft(&mut self, depth: u32) -> u64
		{
			if depth == 0
			{
				1
			}
			else
			{
				let mut count = 0u64;
				for mov in Vec::from(self.legal_moves())
				{
					let played_move = self.make_move(mov);
					count += self.perft(depth - 1);
					self.unmake_move(&played_move);
				}

				count
			}
		}
	}
}
