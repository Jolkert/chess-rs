use rand::seq::SliceRandom;

use crate::board::{moves::Move, Board};

pub struct Engine;
impl Engine
{
	#[allow(clippy::unused_self)]
	pub fn generate_move(&self, board: &Board) -> Move
	{
		*board.legal_moves().choose(&mut rand::thread_rng()).unwrap()
	}
}
