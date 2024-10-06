use nom::{
    branch::alt,
    bytes::complete::{tag, take_until, take_while, take_while1},
    character::complete::digit1,
    combinator::opt,
    error::Error as NomError,
    multi::many0,
    sequence::{pair, preceded},
    IResult,
};

#[derive(Debug, PartialEq)]
pub enum UCICommand {
    UCI,
    Debug(bool),
    IsReady,
    Position {
        fen: Option<String>,
        moves: Vec<String>,
    },
    Go {
        wtime: Option<u64>,
        btime: Option<u64>,
        movestogo: Option<u64>,
        movetime: Option<u64>,
        depth: Option<u64>,
        nodes: Option<u64>,
    },
    Quit,
}

// Helper function to parse an integer
pub fn parse_u32(input: &str) -> IResult<&str, u32> {
    let (input, digits) = digit1(input)?;
    let parsed_int = digits.parse::<u32>().unwrap();
    Ok((input, parsed_int))
}

// Parses an optional parameter of the format "key value"
pub fn parse_go_param(input: &str) -> IResult<&str, (&str, u32)> {
    pair(
        take_while1(|c: char| c.is_alphabetic()),
        preceded(tag(" "), parse_u32),
    )(input)
}

pub fn parse_go(input: &str) -> IResult<&str, UCICommand> {
    let (input, _) = tag("go")(input)?;
    let (input, params) = many0(preceded(tag(" "), parse_go_param))(input)?;

    // Create a vector to store parsed parameters directly
    let mut wtime = None;
    let mut btime = None;
    let mut movestogo = None;
    let mut movetime = None;
    let mut depth = None;
    let mut nodes = None;

    // Update fields directly from params
    for (key, value) in params {
        match key {
            "wtime" => wtime = Some(value as u64),
            "btime" => btime = Some(value as u64),
            "movestogo" => movestogo = Some(value as u64),
            "movetime" => movetime = Some(value as u64),
            "depth" => depth = Some(value as u64),
            "nodes" => nodes = Some(value as u64),
            _ => {}
        }
    }

    Ok((
        input,
        UCICommand::Go {
            wtime,
            btime,
            movestogo,
            movetime,
            depth,
            nodes,
        },
    ))
}

pub fn parse_quit(input: &str) -> IResult<&str, UCICommand> {
    let (input, _) = tag("quit")(input)?;
    Ok((input, UCICommand::Quit))
}

pub fn parse_uci(input: &str) -> IResult<&str, UCICommand> {
    let (input, _) = tag("uci")(input)?;
    Ok((input, UCICommand::UCI))
}

pub fn parse_isready(input: &str) -> IResult<&str, UCICommand> {
    let (input, _) = tag("isready")(input)?;
    Ok((input, UCICommand::IsReady))
}

fn parse_fen(input: &str) -> IResult<&str, String> {
    let (input, fen) =
        take_until(" moves")(input).or_else(|_: nom::Err<NomError<&str>>| Ok((input, input)))?;
    Ok((input, fen.trim().to_string()))
}

fn parse_moves(input: &str) -> IResult<&str, Vec<String>> {
    let (input, moves) = opt(preceded(tag(" moves "), take_while(|c| c != '\n')))(input)?;

    let move_list = moves
        .map(|m| m.split_whitespace().map(|s| s.to_string()).collect())
        .unwrap_or_else(Vec::new);

    Ok((input, move_list))
}

pub fn parse_position(input: &str) -> IResult<&str, UCICommand> {
    // Match either 'startpos' or 'fen <FEN string>'
    let (input, is_startpos) = opt(tag("position startpos"))(input)?;

    let (input, fen) = if is_startpos.is_some() {
        Ok((input, None)) // 'startpos' detected, no FEN string
    } else {
        // Parse 'position fen <FEN string>'
        let (input, _) = tag("position fen ")(input)?;
        let (input, fen) = parse_fen(input)?;
        Ok((input, Some(fen)))
    }?;

    // Now parse the moves if present
    let (input, moves) = parse_moves(input)?;

    Ok(("", UCICommand::Position { fen, moves }))
}

pub fn parse(input: &str) -> IResult<&str, UCICommand> {
    alt((
        parse_uci,
        parse_isready,
        parse_position,
        parse_go,
        parse_quit,
    ))(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_uci() {
        let input = "uci";
        let expected = UCICommand::UCI;
        let result = parse(input);
        assert_eq!(result, Ok(("", expected)));
    }

    #[test]
    fn test_parse_isready() {
        let input = "isready";
        let expected = UCICommand::IsReady;
        let result = parse(input);
        assert_eq!(result, Ok(("", expected)));
    }

    #[test]
    fn test_parse_startpos_only() {
        let input = "position startpos";
        let expected = UCICommand::Position {
            fen: None,
            moves: Vec::new(),
        };
        let result = parse(input);
        assert_eq!(result, Ok(("", expected)));
    }

    #[test]
    fn test_parse_position_with_fen() {
        let input = "position fen rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -";
        let expected = UCICommand::Position {
            fen: Some("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -".to_string()),
            moves: Vec::new(),
        };
        let result = parse(input);
        assert_eq!(result, Ok(("", expected)));
    }

    #[test]
    fn test_parse_position_with_moves() {
        let input = "position startpos moves e2e4 e7e5";
        let expected = UCICommand::Position {
            fen: None,
            moves: vec!["e2e4".to_string(), "e7e5".to_string()],
        };
        let result = parse(input);
        assert_eq!(result, Ok(("", expected)));
    }

    #[test]
    fn test_parse_go() {
        let input = "go depth 5";
        let expected = UCICommand::Go {
            wtime: None,
            btime: None,
            movestogo: None,
            movetime: None,
            depth: Some(5),
            nodes: None,
        };
        let result = parse_go(input);
        assert_eq!(result, Ok(("", expected)));
    }
}
