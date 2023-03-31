#lang racket
;data structure containing the information of a single piece. The name, color and sprite to display
(struct piece (name color sprite))
;an extension of the previous data structure which holds the piece's position on the board in terms of columns and rows
(struct piecePos (piece column row))
;the following are representations of the various types of pieces in chess for easy use later on
(define whiteRook(piece "rook" "white" "white_rook.png"))
(define blackRook (piece "rook" "black" "black_rook.png"))
(define whiteKnight (piece "knight" "white" "white_knight.png"))
(define blackKnight (piece "knight" "black" "black_knight.png"))
(define whiteBishop (piece "bishop" "white" "white_bishop.png"))
(define blackBishop (piece "bishop" "black" "black_bishop.png"))
(define whiteQueen (piece "queen" "white" "white_queen.png"))
(define blackQueen (piece "queen" "black" "black_queen.png"))
(define whiteKing (piece "king" "white" "white_king.png"))
(define blackKing (piece "king" "black" "black_king.png"))
(define whitePawn (piece "pawn" "white" "white_pawn.png"))
(define blackPawn (piece "pawn" "black" "black_pawn.png"))

;This class contains the current setup of the chessboard and the means to change the state of the board through movement of pieces
(define chess% (class object%
    (super-new)
    ;The boardstate is the starting position of the board and any changes are made assuming this as said starting point
    ;It is a list containing the position of a typical initial chessboard setup
    [init-field (boardState (list
        (piecePos whiteRook "A" "1")
        (piecePos whiteKnight "B" "1")
        (piecePos whiteBishop "C" "1")
        (piecePos whiteQueen "D" "1")
        (piecePos whiteKing "E" "1")
        (piecePos whiteBishop "F" "1")
        (piecePos whiteKnight "G" "1")
        (piecePos whiteRook "H" "1")
        (piecePos whitePawn "A" "2")
        (piecePos whitePawn "B" "2")
        (piecePos whitePawn "C" "2")
        (piecePos whitePawn "D" "2")
        (piecePos whitePawn "E" "2")
        (piecePos whitePawn "F" "2")
        (piecePos whitePawn "G" "2")
        (piecePos whitePawn "H" "2")

        (piecePos blackPawn "A" "7")
        (piecePos blackPawn "B" "7")
        (piecePos blackPawn "C" "7")
        (piecePos blackPawn "D" "7")
        (piecePos blackPawn "E" "7")
        (piecePos blackPawn "F" "7")
        (piecePos blackPawn "G" "7")
        (piecePos blackPawn "H" "7")
        (piecePos blackRook "A" "8")
        (piecePos blackKnight "B" "8")
        (piecePos blackBishop "C" "8")
        (piecePos blackQueen "D" "8")
        (piecePos blackKing "E" "8")
        (piecePos blackBishop "F" "8")
        (piecePos blackKnight "G" "8")
        (piecePos blackRook "H" "8")
    ))]
    ;The cells field is used to convert various rows and columns to their pixel by pixel counterpart when overlayed onto the chessboard
    [init-field (cells (hash 
        "A" 77
        "B" 55
        "C" 33
        "D" 11
        "E" -11
        "F" -33
        "G" -55
        "H" -77
        "1" -78
        "2" -56
        "3" -34
        "4" -12
        "5" 10
        "6" 32
        "7" 54
        "8" 76
    ))]

    ;Determines whether a given piece "checkPiece" exists in the board at the specified col and row
    (define/private checkPieceAtPos (lambda (checkPiece board col row)
        (cond
            [(empty? board) #f]
            [(and (equal? (piece-name (piecePos-piece (first board))) (piece-name checkPiece)) (equal? (piecePos-column (first board)) col) (equal? (piecePos-row (first board)) row))#t]
            [#t (checkPieceAtPos checkPiece (rest board) col row)]
        )
    ))
    ;Returns the piece at a given col and row and if there exists no piece then returns #f
    (define/private getPieceAtPos (lambda (board col row)
        (cond
            [(empty? board) #f]
            [(and (equal? (piecePos-column (first board)) col) (equal? (piecePos-row (first board)) row)) (first board)]
            [#t (getPieceAtPos (rest board) col row)]
        )
    ))
    ;Updates the board to reflect any changes made
    (define/private updateBoard (lambda (newBoard)
        (set! boardState newBoard)
    ))
    ;Similar to checkPieceAtPos but instead of requiring and checking that a specific piece exists, this checks if any piece exists at col and row
    (define/private existsAtPos (lambda (board col row)
        (cond
            [(empty? board) #f]
            [(and (equal? (piecePos-column (first board)) col) (equal? (piecePos-row (first board)) row)) #t]
            [#t (existsAtPos (rest board) col row)]
        )
    ))
    (define/private convertRowToFEN (lambda (rowOfPieces)
        (let ((colValues (hash
            "A" 0
            "B" 1
            "C" 2
            "D" 3
            "E" 4
            "F" 5
            "G" 6
            "H" 7
        ))
        (toFEN (hash
            "rook" "r"
            "knight" "n"
            "bishop" "b"
            "queen" "q"
            "king" "k"
            "pawn" "p"
        ))
        (str ""))
        (cond
            [(empty? rowOfPieces) "8"]
            [#t (for ([i (length rowOfPieces)])
                (cond
                    [(equal? (piecePos-column (list-ref rowOfPieces i)) "A") (set! str (string-append str (determineColor (piece-color (piecePos-piece (list-ref rowOfPieces i))) (hash-ref toFEN (piece-name (piecePos-piece (list-ref rowOfPieces i)))))))]
                    [(and (not (equal? (piecePos-column (list-ref rowOfPieces i)) "A")) (equal? i 0)) (set! str (string-append str (determineDifference (hash-ref colValues "A") (hash-ref colValues (piecePos-column (list-ref rowOfPieces i))) #t) (determineColor (piece-color (piecePos-piece (list-ref rowOfPieces i))) (hash-ref toFEN (piece-name (piecePos-piece (list-ref rowOfPieces i)))))))]
                    [#t (set! str (string-append str (determineDifference (hash-ref colValues (piecePos-column (list-ref rowOfPieces (- i 1)))) (hash-ref colValues (piecePos-column (list-ref rowOfPieces i))) #f) (determineColor (piece-color (piecePos-piece (list-ref rowOfPieces i))) (hash-ref toFEN (piece-name (piecePos-piece (list-ref rowOfPieces i)))))))]
                )
                #|
                (cond
                    [(equal? (last rowOfPieces) (list-ref rowOfPieces i)) (set! str (string-append str (determineColor (piece-color (piecePos-piece (list-ref rowOfPieces i))) (hash-ref toFEN (piece-name (piecePos-piece (list-ref rowOfPieces i)))))))]
                    [(and (not (equal? (piecePos-column (list-ref rowOfPieces i)) "A")) (equal? i 0)) (set! str (string-append str (determineColor (piece-color (piecePos-piece (list-ref rowOfPieces i)))(determineDifference (hash-ref colValues "A") (hash-ref colValues (piecePos-column (list-ref rowOfPieces i)))))))]
                    [#t (set! str (string-append str  (determineColor (piece-color (piecePos-piece (list-ref rowOfPieces i))) (hash-ref toFEN (piece-name (piecePos-piece (list-ref rowOfPieces i))))) (determineDifference (hash-ref colValues (piecePos-column (list-ref rowOfPieces i))) (hash-ref colValues (piecePos-column(list-ref rowOfPieces (+ 1 i)))) )))]
                )|#
            )str]
        )
            
        )
    ))

    (define/private determineDifference (lambda (oPiece cPiece start)
        (cond
            [(equal? start #t) (number->string (abs (- oPiece cPiece)))]
            [(equal? (- (abs (- oPiece cPiece)) 1) 0) ""]
            [#t (number->string (- (abs (- oPiece cPiece))1))]
        )
    ))

    (define/private determineColor (lambda (color pieceName)
        (cond
            [(equal? color "white") (string-upcase pieceName)]
            [#t pieceName]
        )
    ))

    (define/public createFEN (lambda ()
        (let ((listRow (list "1" "2" "3" "4" "5" "6" "7" "8"))(finalString ""))
            (for ([i (reverse listRow)])
                (cond
                    [(equal? i (first listRow)) (set! finalString (string-append finalString (convertRowToFEN(filter (lambda(x) (equal? (piecePos-row x) i)) boardState))))]
                    [#t (set! finalString (string-append finalString (convertRowToFEN(filter (lambda(x) (equal? (piecePos-row x) i)) boardState)) "/"))]
                )
            )
            (println finalString)
            finalString
        )
        
    ))
    ;converts the column and row information of a piece into the appropriate pixel coordinates
    (define/public translatePiecePosToBoardPos (lambda (cPiece)
        (list (hash-ref cells (piecePos-column cPiece)) (hash-ref cells (piecePos-row cPiece)))
    ))

    (define/public returnPosPx (lambda (x y)
        (list (hash-ref cells x) (hash-ref cells y))
    ))
    ;returns the current state of the board
    (define/public getBoard(lambda ()
        boardState
    ))
    ;moves whatever piece (if any) is found at icolumn irow to column row
    ;if another piece exists at column row then it is deleted. This is assumed to be a capture
    (define/public makeMove (lambda (icolumn irow column row)
        (cond
            [(existsAtPos boardState icolumn irow)
                (let ((foundFirstPiece (getPieceAtPos boardState icolumn irow)) (foundNextPiece (getPieceAtPos boardState column row)))
                    (cond
                        [(equal? foundNextPiece #f)
                            (updateBoard (append (remove foundFirstPiece boardState) (list (piecePos (piecePos-piece foundFirstPiece) column row))))
                        ]
                        [#t (updateBoard (append (remove foundNextPiece (remove foundFirstPiece boardState)) (list (piecePos (piecePos-piece foundFirstPiece) column row))))]
                    )
                )
            ]
        )
    ))
))

(define board (new chess%))
(provide board)
(provide (struct-out piece))
(provide (struct-out piecePos))
