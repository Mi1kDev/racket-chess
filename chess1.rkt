#|Instructions: 
    Left Click to Select and Right Click to Place
    Assumptions Made: Placing on another sprite implies a capture
|#

#lang racket/gui
;import the necessary packages
(require 2htdp/image (only-in pict pict->bitmap))
(require "chessstate2.rkt")

;define a path to the image folder in which sprites are contained for easy use later
(define imgFolder (path->string (build-path (current-directory) "sprites")))
;we create a frame
(define root (new frame% [label "Chess"][width 194][height 268]))

;converts an image object from 2htdp/image to a bitmap
(define img->bmap (lambda (image)
    (pict->bitmap image)
))
;uses provide coordinates to place a chess piece on the board
(define placePiece (lambda (board cPiece coords)
    (overlay/offset cPiece (first coords) (second coords) board)
))
;gets the bitmap of an image from a file, in this case we need the sprites from the image folder
(define getBitmap (lambda (filename)
    (bitmap/file (string-append imgFolder "\\" filename))
))
;loops through a given boardstate and places all chess pieces in the correct position
(define drawBoard (lambda (boardState iBoardState)
    (cond
        [(empty? boardState) (img->bmap iBoardState)]
        [#t (drawBoard (rest boardState) (placePiece iBoardState (getBitmap (piece-sprite (piecePos-piece (first boardState)))) (send board translatePiecePosToBoardPos (first boardState))))]
    )
))

;an extension of the canvas class. Additional functions are added and some overriden
(define ccanvas% (class canvas%
    (super-new)
    ;we keep track of both the starting position or the position of the piece we are going to move as well as the ending position or where we move the piece to
    [init-field (startPos '())]
    [init-field (endPos '())]
    ;left mouse button is used to select a piece and right to place the selected piece
    (define/override (on-event event)
        ;when the left mouse button is pressed and the cursor is within the bounds of the board, we set the startingPos to the column and row returned
        (when (send event button-down? 'left)
            (let ((click (clickHandler (send event get-x) (send event get-y))))
                (cond
                    [(equal? click #f)]
                    [#t (set! startPos click)]
                )
            )
        )
        ;when the right mpise button is pressed and the cursor is within the bounds of the board, we set the endPos to the column and row returned
        (when (send event button-down? 'right)
            (let ((click (clickHandler (send event get-x) (send event get-y))))
                (cond
                    [(equal? click #f)]
                    [#t (set! endPos click)]
                )
                ;if there is a start and end position then we move the pieces using these two positions and then redraw the board to display the change
                (cond
                    [(not(and (equal? endPos '()) (equal? startPos '())))
                        (send board makeMove (car startPos) (cdr startPos) (car endPos) (cdr endPos))
                        (update)
                    ]
                )
            )
        )
        
    )
    ;determines which row or column the cursor was in given its x and y coordinates
    (define/private clickHandler (lambda (x y)
        (let ((coordX "") (coordY "") (finalCoords ""))
            (cond
                [(and (> x 0) (<= x 180) (> y 0) (<= y 183)) (cond
                    [(and (>= x 0) (<= x 22.5)) (set! coordX "A")]
                    [(and (<= x 45) (> x 22.5)) (set! coordX "B")]
                    [(and (<= x 67.5) (> x 45)) (set! coordX "C")]
                    [(and (<= x 90) (> x 67.5)) (set! coordX "D")]
                    [(and (<= x 112.5) (> x 90)) (set! coordX "E")]
                    [(and (<= x 135) (> x 112.5)) (set! coordX "F")]
                    [(and (<= x 157.5) (> x 135)) (set! coordX "G")]
                    [(and (<= x 180)(> x 157.5)) (set! coordX "H")]
                )
                (cond
                    [(and (>= y 0)(<= y 22.875)) (set! coordY "8")]
                    [(and (<= y 45.75) (> y 22.875)) (set! coordY "7")]
                    [(and (<= y 68.625) (> y 45.75)) (set! coordY "6")]
                    [(and (<= y 91.5) (> y 68.625)) (set! coordY "5")]
                    [(and (<= y 114.375) (> y 91.5)) (set! coordY "4")]
                    [(and (<= y 137.25) (> y 114.375)) (set! coordY "3")]
                    [(and (<= y 160.125) (> y 137.25)) (set! coordY "2")]
                    [(and (<= y 183)(> y 160.125)) (set! coordY "1")]
                ) (cons coordX coordY)]
                [else #f]
            )
        )
    ))
    ;redraws the board
    (define/private update (lambda ()
        (send (send this get-dc) draw-bitmap (drawBoard (send board getBoard) (getBitmap "board.png")) 0 0)
    ))
))

;initialize the canvas and draw the board for the first time during its paint-callback
(new ccanvas%
    [parent root]
    [paint-callback (lambda (canvas dc)
        (send dc draw-bitmap (drawBoard (send board getBoard) (getBitmap "board.png")) 0 0)
    )]
)
(define FEN (new button%[parent root][label "CREATE FEN"][callback (lambda (o e)(send fenOutput set-label (send board createFEN)))]))
(define fenOutput (new message% [parent root][label ""][auto-resize #t]))

(send root show #t)