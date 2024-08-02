' Reversi game

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

Dim mBrd$(7) length 8
Dim hBrd$(7) length 8
Dim dBrd$(7) length 8

Dim scor(1)
Dim vMov(31, 1)
Dim mvCnt
Dim toFlip(31, 1)
Dim nFlipTil
Dim arrDir(7, 1)
Dim plMv(1)
Dim pcMv(1)

Dim plTl$ = ""
Dim pcTl$ = ""

Dim plSc
Dim pcSc

Dim pcNotMv

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

Sub upd_Brd(p_Brd, p_UBD_x, p_UBD_y, p_Str$)

    If p_Brd = 0 Then
        mBrd$(p_UBD_y) = LEFT$(mBrd$(p_UBD_y),p_UBD_x) + p_Str$ + RIGHT$(mBrd$(p_UBD_y), 7 - p_UBD_x)
    ElseIf p_Brd = 1 Then
        hBrd$(p_UBD_y) = LEFT$(hBrd$(p_UBD_y),p_UBD_x) + p_Str$ + RIGHT$(hBrd$(p_UBD_y), 7 - p_UBD_x)    
    ElseIf p_Brd = 2 Then
        dBrd$(p_UBD_y) = LEFT$(dBrd$(p_UBD_y),p_UBD_x) + p_Str$ + RIGHT$(dBrd$(p_UBD_y), 7 - p_UBD_x)    
    EndIf

End Sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

Function gCell$(p_Brd, p_UBD_x, p_UBD_y)
    
    If p_Brd = 0 Then
        gCell$ = MID$(mBrd$(p_UBD_y), p_UBD_x + 1, 1)  
    ElseIf p_Brd = 1 Then   
        gCell$ = MID$(hBrd$(p_UBD_y), p_UBD_x + 1, 1)
    Else    
        gCell$ = MID$(dBrd$(p_UBD_y), p_UBD_x + 1, 1)        
    EndIf

End Function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

Sub resetBoard()

    Local x
    
    For x = 0 To 7
        mBrd$(x) = "        "
    Next    

    upd_Brd(0, 3, 3, "X")
    upd_Brd(0, 3, 4, "O")
    upd_Brd(0, 4, 3, "O")
    upd_Brd(0, 4, 4, "X")
    
End Sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

Sub showScreen(p_Brd)

    Local x, y
    cls

    HLINE$ = "   +---+---+---+---+---+---+---+---+"
    Print    "     1   2   3   4   5   6   7   8                -- R E V E R S I --         "
    Print HLINE$
    
    GetScBrd(p_Brd)
    
    If plTl$ = "X" then
        plSc = scor(0)
        pcSc = scor(1)        
    Else
        plSc = scor(1)    
        pcSc = scor(0)    
    EndIf  

    If plSc < 10 Then
        exSc$ = " "
    Else
        exSc$ = ""
    EndIf 

    For y = 0 To 7
        Print " ";Str$(y+1);" ";

        For x = 0 To 7
            Print "| "; gCell$(p_Brd, x, y);" ";            
        Next

        Print "| ";Str$(y+1)
        
        If y = 0 Then
            Print HLINE$;                        "                    POINTS                  "
        ElseIf y = 1 Then
            Print HLINE$;                        "                Player  Computer            "       
        ElseIf y = 2 Then
            Print HLINE$;"                ";plTl$;" =";exSc$;plSc;"   "pcTl$;" = ";pcSc;"           "            
        ElseIf y = 5 Then           
            Print HLINE$;                        "               The last comp move           " 
        ElseIf y = 6 Then
            If pcNotMv = 0 Then
                Print HLINE$;                        "                    ";pcMv(0)+1;"  ";pcMv(1)+1;"                 "
            Else
                Print HLINE$;                    "                     -   -                  "         
            EndIf  
        Else
            Print HLINE$        
        EndIf 

    Next
    Print    "     1   2   3   4   5   6   7   8" 
    'Print    "12345678901234567890123456789012345678901234567890123456789012345678901234567890"
    'Print    "         1         2         3         4         5         6         7         8"
    Print

End Sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

Function enterplTl$()
    Local tile$, result$

    Do
        Input " Do you want to be X or O "; tile$

        tile$ = UCase$(tile$)

    Loop Until (tile$ = "X" Or tile$ = "O")

    If tile$ = "X" Then
        result$ = "XO"
    Else
        result$ = "OX"
    EndIf
    
    enterplTl$ = result$   

End Function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

Function whoGoesFirst$()

    If Int((Rnd * 2)) Then
        whoGoesFirst$ = "computer"
    Else
        whoGoesFirst$ = "player"
    EndIf

End Function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

Function isOnBoard(x, y)

    If (x >= 0 And x <= 7) And (y >= 0 And y <= 7) Then
        isOnBoard = 1
    Else
        isOnBoard = 0
    EndIf

End Function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

Function IsValMv(p_Brd, tile$, p_X_Start, p_Y_Start )

    Local i, x, y  

    If gCell$(p_Brd, p_X_Start, p_Y_Start) <> " " Then
        IsValMv = 0
        Exit Function
    EndIf
    
    upd_Brd(p_Brd, p_X_Start, p_Y_Start, tile$)    
    
    If tile$ = "X" Then
       otherTile$ = "O"
    Else
       otherTile$ = "X"    
    EndIf    
    
    nFlipTil = 0

    For i = 0 To 7    
        x = p_X_Start
        y = p_Y_Start
        
        X_dir = arrDir(i,0)
        Y_dir = arrDir(i,1)
        
        x = x + X_dir
        y = y + Y_dir

        If isOnBoard(x, y) = 0 Then
            goto EndForDir
        EndIf
        
        If gCell$(p_Brd, x, y) = otherTile$ Then
            x = x + X_dir
            y = y + Y_dir   
            
            If isOnBoard(x, y) = 0 Then
                goto EndForDir
            EndIf
            
            Do While gCell$(p_Brd, x, y) = otherTile$            
                x = x + X_dir
                y = y + Y_dir            
                If isOnBoard(x, y) = 0 Then
                    goto EndForDir
                EndIf
            LOOP       

            If gCell$(p_Brd, x, y) = tile$ Then
    
                Do While 1            
                    x = x - X_dir
                    y = y - Y_dir 
                    
                    If ((x = p_X_Start) And (y = p_Y_Start)) Then
                        Exit DO
                    EndIf
                    
                    toFlip(nFlipTil, 0) = x 
                    toFlip(nFlipTil, 1) = y 
                    
                    nFlipTil = nFlipTil + 1
                    
                LOOP
            EndIf
        EndIf

    EndForDir:        
        
    Next
    
    upd_Brd(p_Brd, p_X_Start, p_Y_Start, " ")      
    
    IsValMv = nFlipTil

End Function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

Function getvMov(p_Brd, tile$)

    Local x_Pos, y_Pos, movCnt
    movCnt = 0

    For x_Pos = 0 To 7
        For y_Pos = 0 To 7

            If IsValMv(p_Brd, tile$, x_Pos, y_Pos) <> 0 Then

                vMov(movCnt, 0) = x_Pos
                vMov(movCnt, 1) = y_Pos
                
                movCnt = movCnt + 1

            EndIf

        Next
    Next

    getvMov = movCnt

End Function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

Sub getBoardCopy(p_Brd)

    Local i, j, tmp_Cell$

    For i = 0 To 7
        For j = 0 To 7
        
            tmp_Cell$ = gCell$(0, i, j)
            upd_Brd(p_Brd, i, j, tmp_Cell$)
        
        Next
    Next            

End Sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

Sub getBoardWithvMov(tile$)

    Local i, x, y

    getBoardCopy(1)

    mvCnt = getvMov(1, tile$)

    For i = 0 To mvCnt - 1

        x = vMov(i, 0)
        y = vMov(i, 1)

        upd_Brd(1, x, y, ".")

    Next

End Sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

Sub GetScBrd(p_Brd)

    Local i, j, xscore, oscore

    For i = 0 To 7
        For j = 0 To 7
            If gCell$(p_Brd, i, j) = "X" Then
            
                xscore = xscore + 1

            ElseIf gCell$(p_Brd, i, j) = "O" Then

                oscore = oscore + 1

            EndIf
        Next
    Next

    scor(0) = xscore
    scor(1) = oscore

End Sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

Function makeMove(p_Brd, tile$, xstart, ystart)

    Local i, tilesCount

    tilesCount = IsValMv(p_Brd, tile$, xstart, ystart)

    If tilesCount = 0 Then
        makeMove = 0
        Exit Function
    EndIf

    upd_Brd(p_Brd, xstart, ystart, tile$)

    For i = 0 To tilesCount - 1
        
        x = toFlip(i, 0)
        y = toFlip(i, 1)        

        upd_Brd(p_Brd, x, y, tile$)

    Next

    makeMove = 1

End Function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

Function getplMv(p_Brd, plTl$)

    Do While 1
        Input "Make move 'x,y'; or '9' - end the game; ENTER - on/off hints : ",tilePos_x, tilePos_y  

        If (tilePos_y >= 0 And tilePos_y <= 8) And (tilePos_x >= 0 And tilePos_x <= 9) Then
        
            If tilePos_y = 0 Then
                If tilePos_x = 0 Then
                    getplMv = 0
                    Exit Function                
                ElseIf tilePos_x = 9 Then
                    getplMv = 9
                    Exit Function
                EndIf
                goto ErrMes
            Else
                If tilePos_x = 0 Then goto ErrMes EndIf         
                       
                If IsValMv(p_Brd, plTl$, tilePos_x - 1, tilePos_y - 1) = 0 Then                  
                    goto ErrMes
                EndIf                    

                plMv(0) = tilePos_x - 1
                plMv(1) = tilePos_y - 1 

                getplMv = 1
                Exit Function
   
            EndIf
        EndIf

        ErrMes:
        Print "That is not a valid move. ";
        Input "Press ENTER ", Pausing        
        
        If showHints = 0 Then
            showScreen(1)
        Else
            showScreen(0)
        EndIf        

    LOOP

End Function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

Function isOnCorner(x, y)
    If ((x = 0 And y = 0) Or (x = 7 And y = 0) Or (x = 0 And y = 7) Or (x = 7 And y = 7)) Then
        isOnCorner = 1
    Else
        isOnCorner = 0
    EndIf

End Function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

Sub randomShuffle(p_Count)

    Local i

    For i = 0 To (Int(Rnd * p_Count) + 5)
    
        firstElem = Int(Rnd * p_Count)
        secondElem = Int(Rnd * p_Count)
        
        temp_X = vMov(secondElem, 0)
        temp_Y = vMov(secondElem, 1)

        vMov(secondElem, 0) = vMov(firstElem, 0)
        vMov(secondElem, 1) = vMov(firstElem, 1) 

        vMov(firstElem, 0) = temp_X
        vMov(firstElem, 1) = temp_Y        
    
    Next

End Sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

Sub getComputerMove(p_Brd, pcTl$)

    Local i, x, y

    possibleMoves = GetvMov(p_Brd, pcTl$)
    randomShuffle(possibleMoves)
      
    For i = 0 To possibleMoves - 1

        x = vMov(i, 0)
        y = vMov(i, 1)    

        If isOnCorner(x, y) = 1 Then

            pcMv(0) = x
            pcMv(1) = y

            Exit Sub

        EndIf
    Next

    bestScore = -1

    For i = 0 To possibleMoves - 1

        getBoardCopy(2)
        
        x = vMov(i, 0)
        y = vMov(i, 1)
        
        movRet = makeMove(2, pcTl$, x, y)

        GetScBrd(2)
        
        If plTl$ = "X" then
            plSc = scor(0)
            pcSc = scor(1)        
        Else
            plSc = scor(1)    
            pcSc = scor(0)    
        EndIf          
        
        If pcSc > bestScore Then

            pcMv(0) = x
            pcMv(1) = y

            bestScore = pcSc

        EndIf
    Next
            
End Sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

arrDir(0, 0) = 0
arrDir(0, 1) = 1

arrDir(1, 0) = 1
arrDir(1, 1) = 1

arrDir(2, 0) = 1
arrDir(2, 1) = 0

arrDir(3, 0) = 1
arrDir(3, 1) = -1

arrDir(4, 0) = 0
arrDir(4, 1) = -1

arrDir(5, 0) = -1
arrDir(5, 1) = -1

arrDir(6, 0) = -1
arrDir(6, 1) = 0

arrDir(7, 0) = -1
arrDir(7, 1) = 1

gameBEGIN:

    resetBoard()
    
    plSc = 0
    pcSc = 0
    pcNotMv = 1

    plTl$ = ""
    pcTl$ = ""
    
    cls
    Print
    Print(" Welcome to Reversi!")
    Print    
    
    getTiles$ = enterplTl$()
    
    plTl$ = LEFT$(getTiles$,1)
    pcTl$ = MID$(getTiles$,2,1)
    
    showHints = 1
    
    turn$ = whoGoesFirst$()
    Print 
    Print " The "; turn$; " will go first."
    Input " Press ENTER ", Pausing
    
    DO While 1
    
        If turn$ = "player" Then
            
            If showHints = 0 Then
                getBoardWithvMov(plTl$)
                showScreen(1)
            Else
                showScreen(0)
            EndIf
            
            movePlayer = getplMv(0, plTl$)
            
            If movePlayer = 9 Then
                Print "Thanks for playing!"
                goto theEND
            ElseIf movePlayer = 0 Then
                If showHints = 0 Then
                    showHints = 1
                Else
                    showHints = 0
                EndIf
                goto endWhileRev
            
            Else           
                movRet = makeMove(0, plTl$, plMv(0), plMv(1))
            
                If (getvMov(0, pcTl$) = 0) Then
                    goto theEND
                Else
                    turn$ = "computer"
                EndIf
            EndIf                    
        
        Else          
        
            showScreen(0)
    
            Input "Press ENTER to see the computer's move ", pressENTER;
            Print "... Waiting ... ";
            
            getComputerMove(0, pcTl$)
            
            movRet = makeMove(0, pcTl$, pcMv(0), pcMv(1))
            pcNotMv = 0
            
            If (getvMov(0, plTl$)) = 0 Then
                goto theEND
            Else
                turn$ = "player"
            EndIf
    
        EndIf
    
        endWhileRev:
    LOOP
    
    ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
    
    theEND:
    
    showScreen(0)
    GetScBrd(0)
    
    If plTl$ = "X" then
        plSc = scor(0)
        pcSc = scor(1)        
    Else
        plSc = scor(1)    
        pcSc = scor(0)    
    EndIf  
    
    If plSc > pcSc Then
        Print "You beat the computer by "; plSc - pcSc; " points! Congratulations!"
    ElseIf plSc < pcSc Then
        Print "You lost. The computer beat you by "; pcSc - plSc; " points."
    Else
        Print "The game was a tie!"
    EndIf 

    Input "  '9' for quit, ENTER for continue: ", Pausing

    If Pausing <> 9 Then
        goto gameBEGIN
    EndIf 

End
