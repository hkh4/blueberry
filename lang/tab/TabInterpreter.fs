module TabInterpreter

open System
open System.IO
open TabTypes
open TabParser
open TabOptions
open Charts
open Music



(* Print out to the postscript file
1) text is the string to print
2) outFile is the name of the file to be written to
RETURNS 0 if succesful and 1 if error
*)
let show text outFile =

   let fullText = "
   %!PS
   %%BeginProlog

   /composer {
   1 dict begin gsave
   /str exch def
   /Times-Roman findfont
   10 scalefont
   setfont
   newpath
   0 0 0 setrgbcolor
   565 705 moveto
   str stringwidth pop -1 mul 0 rmoveto
   str show
   grestore end
   } bind def

   /capo {
   1 dict begin gsave
   /str exch def
   /Times-Roman findfont
   10 scalefont
   setfont
   newpath
   0 0 0 setrgbcolor
   40 705 moveto
   str show
   grestore end
   } bind def

   /tuning {
   1 dict begin gsave
   /str exch def
   /Times-Roman findfont
   10 scalefont
   setfont
   newpath
   0 0 0 setrgbcolor
   40 690 moveto
   str show
   grestore end
   } bind def

   /centerText {
      dup stringwidth pop -0.5 mul 0 rmoveto show } def

   /title {
      1 dict begin gsave
      /str exch def
      /Times-Roman findfont
      22 scalefont setfont
      newpath
      0 0 0 setrgbcolor
      318 730 moveto
      str centerText
      grestore end
   } bind def

   /drawBarre { % given: x, y, fret, start string, end string
      8 dict begin gsave
      /endString exch def
      /startString exch def
      /fret exch def
      /y1 exch def
      /x1 exch def
      /barreLength endString startString sub def
      /yStart 5 fret sub 10 mul 5 add y1 add def
      /xStart startString 1 sub 10 mul x1 add def
      xStart yStart moveto
      1 setlinecap
      5.5 setlinewidth
      10 barreLength mul 0 rlineto stroke
      grestore end
   } bind def

   /drawSpot { % given: x, y, fret, string
      6 dict begin gsave
      /spotString exch def
      /fret exch def
      /y1 exch def
      /x1 exch def
      /yStart 5 fret sub 10 mul 5 add y1 add def
      /xStart spotString 1 sub 10 mul x1 add def
      xStart yStart moveto
      1 setlinecap
      6.5 setlinewidth
      0 0 rlineto stroke
      grestore end
   } bind def

   /chart { % given: x, y, fret, title, fullbarre (1 yes else no - if the barre starts on string 1, move the fret number a little to the left)
      6 dict begin gsave
      0.5 setlinewidth
      /fullBarre exch def
      /title exch def
      /fret exch def
      /y1 exch def
      /x1 exch def
      x1 y1 moveto
      0 1 5 {
         /num exch def
         x1 y1 num 10 mul add moveto
         50 0 rlineto
         x1 num 10 mul add y1 moveto
         0 50 rlineto
         stroke
      } for

      /Helvetica findfont
      8 scalefont setfont
      newpath
      0 0 0 setrgbcolor
      x1 y1 9 sub moveto
      (E) centerText

      newpath
      0 0 0 setrgbcolor
      x1 10 add y1 9 sub moveto
      (A) centerText

      newpath
      0 0 0 setrgbcolor
      x1 20 add y1 9 sub moveto
      (D) centerText

      newpath
      0 0 0 setrgbcolor
      x1 30 add y1 9 sub moveto
      (G) centerText

      newpath
      0 0 0 setrgbcolor
      x1 40 add y1 9 sub moveto
      (B) centerText

      newpath
      0 0 0 setrgbcolor
      x1 50 add y1 9 sub moveto
      (E) centerText

      % title
      newpath
      0 0 0 setrgbcolor
      x1 25 add y1 20 sub moveto
      title centerText

      % fret
      newpath
      0 0 0 setrgbcolor
      fullBarre 1 eq
      {
      /x1 x1 2 sub store
      }{} ifelse
      x1 6 sub y1 42 add moveto
      fret centerText

      grestore end
   } bind def

   /drawOX {
   4 dict begin gsave
   /sym exch def
   /fret exch def
   /y1 exch def
   /x1 exch def

   /Helvetica findfont
   8 scalefont setfont
   newpath
   0 0 0 setrgbcolor
   10 fret 1 sub mul x1 add y1 53 add moveto
   sym centerText

   grestore end
   } bind def

   /drawOX {
   4 dict begin gsave
   /sym exch def
   /fret exch def
   /y1 exch def
   /x1 exch def

   /Helvetica findfont
   8 scalefont setfont
   newpath
   0 0 0 setrgbcolor
   10 fret 1 sub mul x1 add y1 53 add moveto
   sym centerText

   grestore end
   } bind def

   /line {
   4 dict begin gsave
   /ar exch def
   /y1 exch def
   /x1 exch def

   /Times-Roman findfont
   12 scalefont setfont
   newpath
   0 0 0 setrgbcolor

   /skipLoop false def

   x1 y1 moveto

   0 1 ar length 1 sub {

      skipLoop true eq {
         /skipLoop false store

      }{

         /i exch def
         /current ar i get def

         1 current eq {

            /newCurrent ar i 1 add get def

            /currentX currentpoint pop def
            /currentY currentpoint exch pop def

            0 12.5 rmoveto

            % bold
            /Times-Bold findfont
            9 scalefont setfont

            newCurrent show
            currentX currentY moveto
            /skipLoop true store
            /Times-Roman findfont
            12 scalefont setfont

         } {

            current (\sm) eq {

               % new line
               currentpoint exch pop 72 lt {
                  showpage
                  72 720 moveto
               }{
                  72 currentpoint exch pop 15 sub moveto
               } ifelse

            }{

               current (\n) eq {

                  % new line
                  currentpoint exch pop 72 lt {
                     showpage
                     72 720 moveto
                  }{
                     72 currentpoint exch pop 25 sub moveto
                  } ifelse

               }{

                  % if it needs a new line, do so
                  currentpoint pop current stringwidth pop add 540 gt {

                     currentpoint exch pop 72 lt {
                        showpage
                        72 720 moveto
                     }{
                        x1 currentpoint exch pop 25 sub moveto
                     } ifelse

                     current show

                  }{
                     current show
                  } ifelse

               } ifelse

            } ifelse


         } ifelse


      } ifelse

   } for

   grestore end
   } bind def " + text + " showpage %%EndProlog "

   try
      File.WriteAllText(outFile+".ps", fullText)
      0
   with
   | _ ->
      printfn "Error when writing to file"
      1




//******************* DRIVER
(* Main eval method
1) parsed is the result of the parser
2) outfile is the name of the file to be written to
*)
let tabEval parsed outFile =

   // deconstruct
   let (opts, charts, music) = parsed

   // parse the options
   match evalOption opts defaultOptionsRecord with
   | Some(optionsR, optionsText) ->

      match evalCharts charts "" chartStartX chartStartY 0 1 with
      | Some(chartText, x, y) ->

         match evalMusic music x y with
         | Some(musicText) ->

            match show (optionsText + chartText + musicText) outFile with
            | 0 -> 0
            | _ -> 1

         | None -> 1
      | None -> 1
   | None -> 1

































//























//
