module ScoreInterpreter

open System
open System.IO
open ScoreParser
open Types
open Options
open Divide
open Graphics


(* Driver for creating text for postscript file. Every method from here appends onto the base text, which is all the functions and other variables needed
1) pages is list of Pages to be evaluated
2) updatedPages is the list of new pages that have been updated
3) starterText is the string of the postscript functions
4) text is the text that will be updated then printed to postscript file
5) outFile is the name of the file to be printed to
6) list of numbers for tuning
RETURNS string to be printed and the new Page List
*)
let rec show (pages: Page List) (updatedPages: Page List) (starterText: string) (text: string) (outFile: string) (tuningNumbers: int List) : (string * Page List) option =
   match pages with
   // Base: no more pages, print the text to a file called score.ps
   | [] ->
      let fullText = starterText + text
      File.WriteAllText(outFile+".ps",fullText)
      Some(fullText,updatedPages)
   // Recursive case
   | head::tail ->
      let lines = head.Lines
      // Show the lines of a page
      // property list for showing properties
      let defaultPropertyList =
         {
            SlurStart = ((0.0,0.0),false,false);
            MuteStart = ((0.0,0.0),false)
            TieStart = Map.empty.
               Add(1,((0.0,0.0),0,false,false)).
               Add(2,((0.0,0.0),0,false,false)).
               Add(3,((0.0,0.0),0,false,false)).
               Add(4,((0.0,0.0),0,false,false)).
               Add(5,((0.0,0.0),0,false,false)).
               Add(6,((0.0,0.0),0,false,false));
            SlideStart = Map.empty.
               Add(1,((0.0,0.0),0,false,false)).
               Add(2,((0.0,0.0),0,false,false)).
               Add(3,((0.0,0.0),0,false,false)).
               Add(4,((0.0,0.0),0,false,false)).
               Add(5,((0.0,0.0),0,false,false)).
               Add(6,((0.0,0.0),0,false,false));
            SlideStubs = Map.empty.
               Add(1,((0.0,0.0),0,false,false)).
               Add(2,((0.0,0.0),0,false,false)).
               Add(3,((0.0,0.0),0,false,false)).
               Add(4,((0.0,0.0),0,false,false)).
               Add(5,((0.0,0.0),0,false,false)).
               Add(6,((0.0,0.0),0,false,false));
            HammerStart = Map.empty.
               Add(1,((0.0,0.0),0,false,false)).
               Add(2,((0.0,0.0),0,false,false)).
               Add(3,((0.0,0.0),0,false,false)).
               Add(4,((0.0,0.0),0,false,false)).
               Add(5,((0.0,0.0),0,false,false)).
               Add(6,((0.0,0.0),0,false,false))
         }
      match (showLines lines [] "" "" defaultPropertyList tuningNumbers) with
      | Some(t,priority,updatedLines) ->
         let newText = text + priority + t + " showpage "
         // update the Page with the new lines
         let newPage = { head with Lines = updatedLines }
         let newUpdatedPages = updatedPages @ [newPage]
         show tail newUpdatedPages starterText newText outFile tuningNumbers
      | None ->
         None




// ********************* DRIVER **************************
let eval optionsList measuresList outFile =

   // default options
   let optionsR = {Time = (4,4); Key = "c"; Capo = 0; Title = "untitled"; Composer = "unknown"; Tuning = "standard"; TuningNumbers = [0;7;2;9;5;0]}

   // First, parse the options
   match (evalOption optionsList optionsR) with

   // If the options are valid, parse the measures
   | Some(newOption) ->

      // create SingleMeasure List
      match (evalAllMeasures measuresList newOption [] {| Time = false; Key = false; Capo = false |}) with
      | Some(list) ->

         // Take SingleMeasure List and use the widths to create list of lines
         //495 is the width of the first line. The rest are 515.
         match (divideLines list [] 495.0) with

         | Some(lines) ->
            // Take Line List and use heights and type to divide into pages
            match (dividePages lines [] (70.0,720.0)) with

            | Some(pages) ->

               let text = "%!PS
               %%BeginProlog
               /concatenate { %given string1 and string2
                  dup length 2 index length add 1 index type
                  /arraytype eq {array}{string} ifelse
                  dup 0 4 index putinterval
                  dup 4 -1 roll length 4 -1 roll putinterval
               } bind def

               /printimage {
               %% stack: xcoord, ycoord, scalex, scaley, sizex, sizey, pathtofile, color
                  8 dict begin
                  /color exch def
                  /pathtofile exch def
                  /sizey exch def
                  /sizex exch def
                  /scaley exch def
                  /scalex exch def
                  /ycoord exch def
                  /xcoord exch def
                  gsave
                  xcoord ycoord translate
                  scalex scaley scale
                  sizex sizey
                  8
                  [sizex 0 0 -1 sizey mul 0 sizey]
                  pathtofile (r) file /DCTDecode filter
                  false
                  color
                  colorimage
                  grestore
                  end
               } bind def

               % Create one number of the time signature.
               /timesignature { % stack: x-coord, y-coord, number
                  7 dict begin
                  /num exch def
                  /ycoord exch def
                  /xcoord exch def
                  /str (images/Time_Signature/0.jpg) def
                  /num2 {num 48 add} bind def
                  num 10 ge {
                  /str (images/Time_Signature/10.jpg) store
                  /tens num 10 idiv 48 add def
                  /ones num 10 mod 48 add def
                  str 22 tens put
                  str 23 ones put
                  xcoord 3 sub ycoord 13.2575758 8.909 125 84 str 3 printimage
                  }{str 22 num2 put
                  xcoord ycoord 7 8.909 66 84 str 3 printimage
                  } ifelse
               } bind def

               %HELPER: draw single horizontal staff line
               /staffline { % xcoord, ycoord, int - 1 means first line, else 0
                  4 dict begin
                  /first exch def
                  /ycoord exch def
                  /xcoord exch def
                  /width 515 def
                  first 1 eq {/width 495 store} {} ifelse
                  xcoord ycoord moveto
                  0.4 setlinewidth
                  width 0 rlineto
                  stroke
                  end
               } bind def

               /tuning {
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

               %Bar line
               /barline { %xcoord, ycoord, height, linewidth
                  4 dict begin
                  /linewidth exch def
                  /height exch def
                  /ycoord exch def
                  /ycoord ycoord 0.2 sub store
                  /xcoord exch def
                  gsave
                  linewidth setlinewidth
                  xcoord ycoord moveto
                  0 height rlineto
                  stroke
                  grestore
                  end
               } bind def

               %Create the lines
               /guitartablines { % x-coord and y-coord of the first measure line bottom left corner, so NOT the extra fancy line, and an int 1 or 0 to say if first line
                  5 dict begin
                  /flag exch def
                  /ycoord exch def
                  /xcoord exch def
                  gsave
                  %first vertical line
                  1.33 setlinewidth
                  xcoord ycoord 30.4 1.33 barline
                  stroke
                  % horizontal lines
                  0.4 setlinewidth
                  0 1 5 {
                     /num exch def
                     xcoord num 6 mul ycoord add flag staffline
                  } for
                  1.33 setlinewidth
                  /width 515 def
                  flag 1 eq {/width 495 store}{} ifelse
                  xcoord width add ycoord 30.4 1.33 barline
                  stroke
                  xcoord ycoord 40 fancyline
                  end
               } bind def

               %create fancy line at the beginning of staff
               /fancyline { % given x coord, y coord, height
                  3 dict begin
                  /height exch def
                  /ycoord exch def
                  /xcoord exch def
                  xcoord 5 sub ycoord 5 sub moveto
                  2.5 setlinewidth
                  0 height rlineto
                  stroke
                  newpath %bottom curl
                  0.1 setlinewidth
                  xcoord 4 sub ycoord 5 sub moveto
                  xcoord 2 sub ycoord 5 sub xcoord 0.5 sub ycoord 5.5 sub xcoord 2 add ycoord 8 sub curveto
                  xcoord ycoord 4.666 sub xcoord 6 sub ycoord 3 sub 10 arct
                  closepath
                  fill
                  newpath %top curl
                  xcoord 4 sub ycoord 5 sub height add moveto
                  xcoord 2 sub ycoord 5 sub height add xcoord 0.5 sub ycoord 4.5 sub height add xcoord 2 add ycoord 2 sub height add curveto
                  xcoord ycoord 5.333 sub height add xcoord 6 sub ycoord 7 sub height add 10 arct
                  closepath
                  fill
               } bind def

               /guitarfretnumber { %xcoord, ycoord, filestring
                  8 dict begin
                  /str exch def
                  /ycoord exch def
                  /xcoord exch def
                  /scalex 4 def
                  /scaley 4.51 def
                  /sizex 800 def
                  /sizey 902 def
                  /filestring (temp) def
                  str type /stringtype eq {
                     /xcoord xcoord 0.4 sub store
                     /filestring (images/Tab_Numbers/) str (.jpg) concatenate concatenate store
                     /scalex 4.6 store
                     /scaley 4.8 store
                     /sizex 1000 store
                     }{
                     str 9 gt {
                        /xcoord xcoord 1.7 sub store
                        /scalex 7.3 store
                        /sizex 1460 store
                        }{} ifelse
                     /filestring (images/Tab_Numbers/) str (ffff) cvs (.jpg) concatenate concatenate store
                     } ifelse
                  xcoord ycoord scalex scaley sizex sizey filestring 1 printimage
                  end
               } bind def

               /quarterRest { %x,y
               2 dict begin
               gsave
               0.1 setlinewidth
               /ycoord exch def
               /xcoord exch def
               /ycoord ycoord 12 add store
               xcoord ycoord moveto
               xcoord 2.8000000000000003 add ycoord 3.857142857142857 sub lineto
               xcoord 0.5714285714285714 add ycoord 6.571428571428571 sub xcoord 1.1428571428571428 add ycoord 7.285714285714286 sub xcoord 3.4285714285714284 add ycoord 9.514285714285714 sub curveto
               xcoord 0.8571428571428571 add ycoord 8.657142857142857 sub xcoord ycoord 10.085714285714285 sub xcoord 1.657142857142857 add ycoord 12.085714285714285 sub curveto
               xcoord 1.657142857142857 add ycoord 12.142857142857142 sub xcoord 1.5714285714285714 add ycoord 12.200000000000001 sub xcoord 1.4857142857142858 add ycoord 12.12857142857143 sub curveto
               xcoord 2.142857142857143 sub ycoord 10.0 sub xcoord 0.19999999999999998 add ycoord 7.428571428571429 sub xcoord 1.7142857142857142 add ycoord 8.285714285714286 sub curveto
               xcoord 0.6571428571428571 sub ycoord 5.257142857142857 sub lineto
               xcoord 1.1428571428571428 add ycoord 3.257142857142857 sub xcoord 1.1428571428571428 add ycoord 2.2857142857142856 sub xcoord 0.24285714285714285 sub ycoord 0.19999999999999998 sub curveto
               xcoord 0.24285714285714285 sub ycoord 0.1142857142857143 sub xcoord 0.14285714285714285 sub ycoord xcoord ycoord curveto
               fill
               grestore
               end
               } bind def

               /restCurl { %given x and y coord
                  2 dict begin
                  gsave
                  0.1 setlinewidth
                  /ycoord exch def
                  /xcoord exch def
                  newpath
                  xcoord ycoord moveto
                  xcoord 0.64816513755 sub ycoord 0.51853211004 sub xcoord 1.058669724665 sub ycoord 0.885825687985 sub xcoord 1.46917431178 sub ycoord 0.99385321091 sub curveto
                  xcoord 2.46302752269 sub ycoord 1.85807339431 sub lineto
                  xcoord 2.03091743099 sub ycoord 1.85807339431 sub xcoord 1.85807339431 sub ycoord 1.77165137597 sub xcoord 0.95064220174 sub ycoord 1.4475688071950001 sub curveto
                  xcoord 0.8642201834000001 sub ycoord 1.42596330261 sub xcoord 0.8642201834000001 sub ycoord 1.663623853045 sub 0.08642201834 arct
                  closepath
                  fill
                  newpath xcoord 2.46302752269 sub ycoord 0.8210091742300001 sub 1.03706422008 0 360 arc fill
                  grestore
                  end
               } bind def

               /8thRest { %given x and y coord
                  2 dict begin
                  gsave
                  0.1 setlinewidth
                  /ycoord exch def
                  /xcoord exch def
                  /xcoord xcoord 1 add store
                  xcoord ycoord moveto
                  xcoord 0.04321100917 add ycoord 0.194449541265 sub xcoord 0.8210091742300001 add ycoord 0.15123853209500002 sub xcoord 0.99385321091 add ycoord curveto
                  xcoord 2.72229357771 add ycoord 6.22238532048 add lineto
                  xcoord 2.72229357771 add ycoord 6.3520183479900005 add xcoord 2.5062385318600002 add ycoord 6.5248623846700005 add xcoord 2.37660550435 add ycoord 6.43844036633 add curveto
                  xcoord 1.51238532095 add ycoord 4.904449540795 add lineto
                  xcoord 1.51238532095 add ycoord 4.58036697202 add xcoord 0.04321100917 sub ycoord 0.12963302751 add xcoord 0.04321100917 sub ycoord 0.12963302751 add curveto
                  xcoord 0.08642201834 sub ycoord xcoord 0.051853211004 add ycoord 0.064816513755 sub 0.17284403668 arct
                  fill
                  xcoord 2.37660550435 add ycoord 6.43844036633 add restCurl
                  grestore
                  end
               } bind def

               /16thRest { %given x and y coord
                  2 dict begin
                  gsave
                  0.1 setlinewidth
                  /ycoord exch def
                  /xcoord exch def
                  xcoord ycoord moveto
                  xcoord 0.04321100917 add ycoord 0.194449541265 sub xcoord 0.8210091742300001 add ycoord 0.15123853209500002 sub xcoord 0.99385321091 add ycoord curveto
                  xcoord 3.9322018344700003 add ycoord 10.15458715495 add lineto
                  xcoord 3.9538073390550004 add ycoord 10.262614677875 add xcoord 3.7377522932050002 add ycoord 10.435458714555 add xcoord 3.62972477028 add ycoord 10.3706422008 add curveto
                  xcoord 2.76550458688 add ycoord 8.836651375265001 add lineto
                  xcoord 1.51238532095 add ycoord 4.58036697202 add xcoord 0.04321100917 sub ycoord 0.12963302751 add xcoord 0.04321100917 sub ycoord 0.12963302751 add curveto
                  xcoord 0.08642201834 sub ycoord xcoord 0.051853211004 add ycoord 0.064816513755 sub 0.17284403668 arct
                  fill
                  xcoord 2.4198165135200003 add ycoord 6.43844036633 add restCurl
                  xcoord 3.62972477028 add ycoord 10.3706422008 add restCurl
                  grestore
                  end
               } bind def

               /32ndRest { %given x and y coord
                  2 dict begin
                  gsave
                  0.1 setlinewidth
                  /ycoord exch def
                  /xcoord exch def
                  xcoord ycoord moveto
                  xcoord 0.04321100917 add ycoord 0.194449541265 sub xcoord 0.8210091742300001 add ycoord 0.15123853209500002 sub xcoord 0.99385321091 add ycoord curveto
                  xcoord 4.515550458265 add ycoord 14.151605503175 add lineto
                  xcoord 4.53715596285 add ycoord 14.17321100776 add xcoord 4.429128439925 add ycoord 14.367660549025 add xcoord 4.23467889866 add ycoord 14.272596328851002 add curveto
                  xcoord 3.41366972443 add ycoord 12.79045871432 add lineto
                  xcoord 1.33954128427 add ycoord 4.58036697202 add xcoord 0.04321100917 sub ycoord 0.12963302751 add xcoord 0.04321100917 sub ycoord 0.12963302751 add curveto
                  xcoord 0.08642201834 sub ycoord xcoord 0.051853211004 add ycoord 0.064816513755 sub 0.17284403668 arct
                  fill
                  xcoord 2.20376146767 add ycoord 6.43844036633 add restCurl
                  xcoord 3.2624311923350002 add ycoord 10.3706422008 add restCurl
                  xcoord 4.277889907830001 add ycoord 14.30284403527 add restCurl
                  grestore
                  end
               } bind def

               /64thRest { %given x and y coord
                  2 dict begin
                  gsave
                  0.1 setlinewidth
                  /ycoord exch def
                  /xcoord exch def
                  xcoord ycoord moveto
                  xcoord 0.04321100917 add ycoord 0.194449541265 sub xcoord 0.8210091742300001 add ycoord 0.15123853209500002 sub xcoord 0.99385321091 add ycoord curveto
                  xcoord 5.53100917376 add ycoord 18.1486238514 add lineto
                  xcoord 5.48779816459 add ycoord 18.27825687891 add xcoord 5.44458715542 add ycoord 18.32146788808 add xcoord 5.250137614155 add ycoord 18.269614677076 add curveto
                  xcoord 4.40752293534 add ycoord 16.76587155796 add lineto
                  xcoord 1.33954128427 add ycoord 4.58036697202 add xcoord 0.04321100917 sub ycoord 0.12963302751 add xcoord 0.04321100917 sub ycoord 0.12963302751 add curveto
                  xcoord 0.08642201834 sub ycoord xcoord 0.051853211004 add ycoord 0.064816513755 sub 0.17284403668 arct
                  fill
                  xcoord 2.20376146767 add ycoord 6.43844036633 add restCurl
                  xcoord 3.24082568775 add ycoord 10.3706422008 add restCurl
                  xcoord 4.277889907830001 add ycoord 14.30284403527 add restCurl
                  xcoord 5.27174311874 add ycoord 18.27825687891 add restCurl
                  grestore
                  end
               } bind def

               /halfWholeRest { %given x and y coord
               2 dict begin gsave 0.1 setlinewidth /ycoord exch def /xcoord exch def
               xcoord 0.37129898 add ycoord moveto
               xcoord 4.48414922 add ycoord xcoord 4.48414922 add ycoord 0.37129898 add 0.37129898 arct
               xcoord 4.48414922 add ycoord 2.31347826 add xcoord 4.11285024 add ycoord 2.31347826 add 0.37129898 arct
               xcoord ycoord 2.31347826 add xcoord ycoord 0.37129898 add 0.37129898 arct
               xcoord ycoord xcoord 0.37129898 add ycoord 0.37129898 arct
               fill
               grestore end
               } bind def

               /drawFlag {
               2 dict begin gsave
               /ycoord exch def
               /xcoord exch def
               0.1 setlinewidth
               xcoord ycoord moveto
               xcoord ycoord 2.744186046511628 add lineto
               xcoord 0.20930232558139536 add ycoord 2.697674418604651 add 0.20930232558139536 170 10 arcn
               xcoord 0.6976744186046512 add ycoord 1.627906976744186 sub xcoord 4.3023255813953485 add ycoord 1.627906976744186 sub xcoord 2.744186046511628 add ycoord 7.1395348837209305 sub curveto
               xcoord 2.604651162790698 add ycoord 7.372093023255814 sub xcoord 2.2325581395348837 add ycoord 7.325581395348837 sub xcoord 2.2093023255813953 add ycoord 6.976744186046512 sub curveto
               xcoord 3.13953488372093 add ycoord 3.953488372093023 sub xcoord 2.488372093023256 add ycoord 2.7906976744186047 sub xcoord ycoord curveto
               fill
               grestore end
               } bind def

               /measureNumber {
                  3 dict begin gsave
                  /str exch def
                  /ycoord exch def
                  /xcoord exch def
                  /Times-Roman findfont
                  7 scalefont setfont
                  newpath
                  0 0 0 setrgbcolor
                  xcoord ycoord moveto
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
                  306 745 moveto
                  str centerText
                  grestore end
               } bind def

               /composer {
               1 dict begin gsave
               /str exch def
               /Times-Roman findfont
               10 scalefont
               setfont
               newpath
               0 0 0 setrgbcolor
               565 720 moveto
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
               40 720 moveto
               str show
               grestore end
               } bind def

               /guitarfretnumbergrace { %xcoord, ycoord, filestring
                  8 dict begin
                  /str exch def
                  /ycoord exch def
                  /xcoord exch def
                  /scalex 2.7 def
                  /scaley 3.04425 def
                  /sizex 800 def
                  /sizey 902 def
                  /filestring (temp) def
                  str type /stringtype eq {
                     /xcoord xcoord 0.4 sub store
                     /filestring (images/Tab_Numbers/) str (.jpg) concatenate concatenate store
                     /scalex 3.105 store
                     /scaley 3.24 store
                     /sizex 1000 store
                     }{
                     str 9 gt {
                        /xcoord xcoord 1.1475 sub store
                        /scalex 4.9275 store
                        /sizex 1460 store
                        }{} ifelse
                     /filestring (images/Tab_Numbers/) str (ffff) cvs (.jpg) concatenate concatenate store
                     } ifelse
                  xcoord ycoord scalex scaley sizex sizey filestring 1 printimage
                  end
               } bind def

               /drawFlagGrace {
               2 dict begin gsave
               /ycoord exch def
               /xcoord exch def
               0.1 setlinewidth
               xcoord ycoord moveto
               xcoord ycoord 2.1953488372093024 add lineto
               xcoord 0.1674418604651163 add ycoord 2.158139534883721 add 0.1674418604651163 170 10 arcn
               xcoord 0.5581395348837209 add ycoord 1.302325581395349 sub xcoord 3.441860465116279 add ycoord 1.302325581395349 sub xcoord 2.1953488372093024 add ycoord 5.711627906976744 sub curveto
               xcoord 2.0837209302325586 add ycoord 5.897674418604652 sub xcoord 1.786046511627907 add ycoord 5.8604651162790695 sub xcoord 1.7674418604651163 add ycoord 5.5813953488372094 sub curveto
               xcoord 2.511627906976744 add ycoord 3.162790697674419 sub xcoord 1.9906976744186047 add ycoord 2.2325581395348837 sub xcoord ycoord curveto
               fill
               grestore end
               } bind def

               /graceCurve {
               4 dict begin gsave
               0.7 setlinewidth
               /y2 exch def
               /x2 exch def
               /y1 exch def
               /x1 exch def
               x1 y1 moveto
               x2 x1 sub 0.13043478 mul x1 add     y1 2.5 add
               x2 x1 sub 0.63043478 mul x1 add     y1 3 add
               x2 y2 curveto stroke
               grestore end
               } bind def

               /slurshort {
               5 dict begin gsave
               0.7 setlinewidth
               /y2 exch def
               /x2 exch def
               /y1 exch def
               /x1 exch def
               /x1 x1 3 add store
               /x2 x2 1 add store
               x1 y1 44 add moveto
               /temp x2 x1 sub 0.3 mul def
               x1 temp add y1 47 add x2 temp sub y2 47 add
               x2 y2 44 add curveto
               stroke
               grestore end
               } bind def

               /slur {
               5 dict begin gsave
               0.7 setlinewidth
               /y2 exch def
               /x2 exch def
               /y1 exch def
               /x1 exch def
               /x1 x1 3 add store
               /x2 x2 1 add store
               x1 y1 44 add moveto
               /temp x2 x1 sub 0.3 mul def
               x1 temp add y1 48.5 add x2 temp sub y2 48.5 add
               x2 y2 44 add curveto
               stroke
               grestore end
               } bind def

               /slurlong {
               5 dict begin gsave
               0.7 setlinewidth
               /y2 exch def
               /x2 exch def
               /y1 exch def
               /x1 exch def
               /x1 x1 3 add store
               /x2 x2 1 add store
               x1 y1 44 add moveto
               /temp x2 x1 sub 0.3 mul def
               x1 temp add y1 51.5 add x2 temp sub y2 51.5 add
               x2 y2 44 add curveto
               stroke
               grestore end
               } bind def

               /slurverylong {
               5 dict begin gsave
               0.7 setlinewidth
               /y2 exch def
               /x2 exch def
               /y1 exch def
               /x1 exch def
               /x1 x1 3 add store
               /x2 x2 1 add store
               x1 y1 44 add moveto
               /temp x2 x1 sub 0.3 mul def
               x1 temp add y1 55 add x2 temp sub y2 55 add
               x2 y2 44 add curveto
               stroke
               grestore end
               } bind def

               /slurgrace {
               5 dict begin gsave
               0.5 setlinewidth
               /y2 exch def
               /x2 exch def
               /y1 exch def
               /x1 exch def
               /x1 x1 1.5 add store
               /x2 x2 1 add store
               x1 y1 41.5 add moveto
               /temp x2 x1 sub 0.3 mul def
               x1 temp add y1 44 add x2 temp sub y2 44 add
               x2 y2 41.5 add curveto
               stroke
               grestore end
               } bind def

               /tie {
               6 dict begin gsave
              0.6 setlinewidth
              /fret exch def
              /y2 exch def
              /x2 exch def
              /y1 exch def
              /x1 exch def
              fret 9 gt {
                 /x1 x1 0.7 add store
                 /x2 x2 0.7 sub store
              }{} ifelse
              /x1 x1 4.2 add store
              /x2 x2 0.2 sub store
              /y2 y2 2.5 add store
              /y1 y1 2.5 add store
              x1 y1 1 add moveto
              /temp x2 x1 sub 0.3 mul def
              x1 temp add y1 3 add x2 temp sub y2 3 add
              x2 y2 1 add curveto
              stroke
              grestore end
               } bind def

               /tiegrace {
               6 dict begin gsave
              0.4 setlinewidth
              /fret exch def
              /y2 exch def
              /x2 exch def
              /y1 exch def
              /x1 exch def
              fret 9 gt {
                 /x1 x1 0.4 add store
                 /x2 x2 0.4 sub store
              }{} ifelse
              /x1 x1 2.9 add store
              /x2 x2 0.2 sub store
              /y2 y2 2.3 add store
              /y1 y1 2.3 add store
              x1 y1 1 add moveto
              /temp x2 x1 sub 0.3 mul def
              x1 temp add y1 2.6 add x2 temp sub y2 2.6 add
              x2 y2 1 add curveto
              stroke
              grestore end
               } bind def

               /tiegracefirst {
               6 dict begin gsave
              0.6 setlinewidth
              /fret exch def
              /y2 exch def
              /x2 exch def
              /y1 exch def
              /x1 exch def
              fret 9 gt {
                 /x1 x1 0.4 add store
                 /x2 x2 0.4 sub store
              }{} ifelse
              /x1 x1 2.9 add store
              /x2 x2 0.2 sub store
              /y2 y2 2.5 add store
              /y1 y1 2.5 add store
              x1 y1 1 add moveto
              /temp x2 x1 sub 0.3 mul def
              x1 temp add y1 3 add x2 temp sub y2 3 add
              x2 y2 1 add curveto
              stroke
              grestore end
               } bind def

               /tiegracesecond {
               6 dict begin gsave
              0.6 setlinewidth
              /fret exch def
              /y2 exch def
              /x2 exch def
              /y1 exch def
              /x1 exch def
              fret 9 gt {
                 /x1 x1 0.4 add store
                 /x2 x2 0.4 sub store
              }{} ifelse
              /x1 x1 4.2 add store
              /x2 x2 0.2 sub store
              /y2 y2 2.5 add store
              /y1 y1 2.5 add store
              x1 y1 1 add moveto
              /temp x2 x1 sub 0.3 mul def
              x1 temp add y1 3 add x2 temp sub y2 3 add
              x2 y2 1 add curveto
              stroke
              grestore end
               } bind def

               /slideup {
               6 dict begin gsave
               /y2 exch def
               /x2 exch def
               /y1 exch def
               /x1 exch def
               /fret2 exch def
               /fret1 exch def
               fret1 9 gt {
                  /x1 x1 1.6 add store
               }{} ifelse
               fret2 9 gt {
                  /x2 x2 1.6 sub store
               }{} ifelse
               0.3 setlinewidth
               x1 4 add y1 0.7 add moveto
               x2 0.2 sub y2 3.8 add lineto
               stroke
               grestore end
               } bind def

               /slidedown {
               6 dict begin gsave
               /y2 exch def
               /x2 exch def
               /y1 exch def
               /x1 exch def
               /fret2 exch def
               /fret1 exch def
               fret2 9 gt {
                  /x2 x2 1.4 sub store
               }{} ifelse
               fret1 9 gt {
                  /x1 x1 1.5 add store
               }{} ifelse
               0.3 setlinewidth
               x1 4 add y1 3.8 add moveto
               x2 0.2 sub y2 0.7 add lineto
               stroke
               grestore end
               } bind def

               /slidedownbothgrace {
               6 dict begin gsave
               /y2 exch def
               /x2 exch def
               /y1 exch def
               /x1 exch def
               /fret2 exch def
               /fret1 exch def
               fret2 9 gt {
                  /x2 x2 1.1 sub store
               }{} ifelse
               fret1 9 gt {
                  /x1 x1 1.1 add store
               }{} ifelse
               0.3 setlinewidth
               x1 3 add y1 3.4 add moveto
               x2 0.2 sub y2 1.2 add lineto
               stroke
               grestore end
               } bind def

               /slideupbothgrace {
               6 dict begin gsave
               /y2 exch def
               /x2 exch def
               /y1 exch def
               /x1 exch def
               /fret2 exch def
               /fret1 exch def
               fret2 9 gt {
                  /x2 x2 1.1 sub store
               }{} ifelse
               fret1 9 gt {
                  /x1 x1 1.1 add store
               }{} ifelse
               0.3 setlinewidth
               x1 3 add y1 1.2 add moveto
               x2 0.2 sub y2 3.4 add lineto
               stroke
               grestore end
               } bind def

               /slidedownfirstgrace {
               6 dict begin gsave
               /y2 exch def
               /x2 exch def
               /y1 exch def
               /x1 exch def
               /fret2 exch def
               /fret1 exch def
               fret2 9 gt {
                  /x2 x2 1.3 sub store
               }{} ifelse
               fret1 9 gt {
                  /x1 x1 1.1 add store
               }{} ifelse
               0.3 setlinewidth
               x1 2.9 add y1 3.4 add moveto
               x2 0.1 sub y2 1.2 add lineto
               stroke
               grestore end
               } bind def

               /slideupfirstgrace {
               6 dict begin gsave
               /y2 exch def
               /x2 exch def
               /y1 exch def
               /x1 exch def
               /fret2 exch def
               /fret1 exch def
               fret2 9 gt {
                  /x2 x2 1.3 sub store
               }{} ifelse
               fret1 9 gt {
                  /x1 x1 1.1 add store
               }{} ifelse
               0.3 setlinewidth
               x1 2.9 add y1 1.2 add moveto
               x2 0.1 sub y2 3.4 add lineto
               stroke
               grestore end
               } bind def

               /slidedownsecondgrace {
               6 dict begin gsave
               /y2 exch def
               /x2 exch def
               /y1 exch def
               /x1 exch def
               /fret2 exch def
               /fret1 exch def
               fret2 9 gt {
                  /x2 x2 1.1 sub store
               }{} ifelse
               fret1 9 gt {
                  /x1 x1 1.6 add store
               }{} ifelse
               0.3 setlinewidth
               x1 3.9 add y1 3.4 add moveto
               x2 0.1 sub y2 1.2 add lineto
               stroke
               grestore end
               } bind def

               /slideupsecondgrace {
               6 dict begin gsave
               /y2 exch def
               /x2 exch def
               /y1 exch def
               /x1 exch def
               /fret2 exch def
               /fret1 exch def
               fret2 9 gt {
                  /x2 x2 1.1 sub store
               }{} ifelse
               fret1 9 gt {
                  /x1 x1 1.6 add store
               }{} ifelse
               0.3 setlinewidth
               x1 3.9 add y1 1.2 add moveto
               x2 0.1 sub y2 3.4 add lineto
               stroke
               grestore end
               } bind def

               /sup {
               5 dict begin gsave
               /fret exch def
               /y1 exch def
               /x1 exch def
               /temp x1 5 sub def
               /temp2 y1 0.7 add def
               fret 9 gt {
                  /x1 x1 0.6 sub store
                  /y1 y1 0.24230769 sub store
               }{} ifelse
               0.3 setlinewidth
               temp temp2 moveto
               x1 0.2 sub y1 3.8 add lineto
               stroke
               grestore end
               } bind def

               /supgrace {
               5 dict begin gsave
               /fret exch def
               /y1 exch def
               /x1 exch def
               /temp x1 3 sub def
               /temp2 y1 1.2 add def
               fret 9 gt {
                  /x1 x1 0.6 sub store
                  /y1 y1 0.4125 sub store
               }{} ifelse
               0.3 setlinewidth
               temp temp2 moveto
               x1 0.2 sub y1 3.4 add lineto
               stroke
               grestore end
               } bind def

               /sdn {
               5 dict begin gsave
               /fret exch def
               /y1 exch def
               /x1 exch def
               /temp x1 5 sub def
               /temp2 y1 3.8 add def
               fret 9 gt {
                  /x1 x1 0.6 sub store
                  /y1 y1 0.24230769 add store
               }{} ifelse
               0.3 setlinewidth
               temp temp2 moveto
               x1 0.2 sub y1 0.7 add lineto
               stroke
               grestore end
               } bind def

               /sdngrace {
               5 dict begin gsave
               /fret exch def
               /y1 exch def
               /x1 exch def
               /temp x1 3 sub def
               /temp2 y1 3.4 add def
               fret 9 gt {
                  /x1 x1 0.6 sub store
                  /y1 y1 0.4125 add store
               }{} ifelse
               0.3 setlinewidth
               temp temp2 moveto
               x1 0.2 sub y1 1.2 add lineto
               stroke
               grestore end
               } bind def

               /openP {
               3 dict begin gsave
               /fret exch def
               /y1 exch def
               /x1 exch def
               fret 9 gt {
                  /x1 x1 1.0 sub store
               }{} ifelse
               /Times-Roman findfont
               7 scalefont
               setfont
               newpath
               0 0 0 setrgbcolor
               x1 y1 moveto
               (\() show
               grestore end
               } bind def

               /closeP {
               3 dict begin gsave
               /fret exch def
               /y1 exch def
               /x1 exch def
               fret 9 gt {
                  /x1 x1 1.1 add store
               }{} ifelse
               /Times-Roman findfont
               7 scalefont
               setfont
               newpath
               0 0 0 setrgbcolor
               x1 y1 moveto
               (\)) show
               grestore end
               } bind def

               /openPG {
               3 dict begin gsave
               /fret exch def
               /y1 exch def
               /x1 exch def
               fret 9 gt {
                  /x1 x1 0.9 sub store
               }{} ifelse
               /Times-Roman findfont
               5 scalefont
               setfont
               newpath
               0 0 0 setrgbcolor
               x1 y1 moveto
               (\() show
               grestore end
               } bind def

               /closePG {
               3 dict begin gsave
               /fret exch def
               /y1 exch def
               /x1 exch def
               fret 9 gt {
                  /x1 x1 0.8 add store
               }{} ifelse
               /Times-Roman findfont
               5 scalefont
               setfont
               newpath
               0 0 0 setrgbcolor
               x1 y1 moveto
               (\)) show
               grestore end
               } bind def

               /arrow {
                 6 dict begin gsave
                 /start1 exch def
                 /height exch def
                 /y1 exch def
                 /x1 exch def
                 /x1 x1 2.2 sub store
                 /y1 y1 8.5 sub store
                 /scale1 start1 6 mul def
                 /y1 y1 scale1 add store
                 /len height 1 sub 6 mul 4.5 add def
                 x1 y1 moveto
                 0.45 setlinewidth
                 0 len rlineto
                 stroke
                 x1 y1 len add moveto
                 1.35 0 rlineto
                 -1.35 3.5 rlineto
                 -1.35 -3.5 rlineto
                 1.35 0 rlineto
                 fill
                 grestore end
               } bind def

               /arrowGrace {
                 6 dict begin gsave
                 /start1 exch def
                 /height exch def
                 /y1 exch def
                 /x1 exch def
                 /x1 x1 1.5 sub store
                 /y1 y1 8.5 sub store
                 /scale1 start1 6 mul def
                 /y1 y1 scale1 add store
                 /len height 1 sub 6 mul 4 add def
                 x1 y1 moveto
                 0.35 setlinewidth
                 0 len rlineto
                 stroke
                 x1 y1 len add moveto
                 1 0 rlineto
                 -1 3 rlineto
                 -1 -3 rlineto
                 1 0 rlineto
                 fill
                 grestore end
               } bind def

               /arrowDown {
                 6 dict begin gsave
                 /start1 exch def
                 /height exch def
                 /y1 exch def
                 /x1 exch def
                 /x1 x1 2.2 sub store
                 /y1 y1 3 sub store
                 /scale1 start1 6 mul def
                 /y1 y1 scale1 add store
                 /len height -6 mul 1 add def
                 x1 y1 moveto
                 0.45 setlinewidth
                 0 len rlineto
                 stroke
                 x1 y1 len add moveto
                 1.35 0 rlineto
                 -1.35 -3.5 rlineto
                 -1.35 3.5 rlineto
                 1.35 0 rlineto
                 fill
                 grestore end
               } bind def

               /arrowDownGrace {
                 6 dict begin gsave
                 /start1 exch def
                 /height exch def
                 /y1 exch def
                 /x1 exch def
                 /x1 x1 1.5 sub store
                 /y1 y1 3.6 sub store
                 /scale1 start1 6 mul def
                 /y1 y1 scale1 add store
                 /len height -6 mul 2 add def
                 x1 y1 moveto
                 0.35 setlinewidth
                 0 len rlineto
                 stroke
                 x1 y1 len add moveto
                 1 0 rlineto
                 -1 -3 rlineto
                 -1 3 rlineto
                 1 0 rlineto
                 fill
                 grestore end
               } bind def

               /harmonics {
                  5 dict begin gsave
                  /fret exch def
                  /y1 exch def
                  /x1 exch def
                  /x1 x1 3.3 sub store
                  /y1 y1 2.3 add store
                  /x2 x1 10.7 add store
                  fret 9 gt {
                     /x1 x1 1.5 sub store
                     /x2 x2 1.9 add store
                  }{} ifelse
                  0.5 setlinewidth
                  x1 y1 moveto
                  x1 3.5 add y1 1.5 add lineto
                  x1 y1 moveto
                  x1 3.5 add y1 1.5 sub lineto
                  x2 y1 moveto
                  x2 3.5 sub y1 1.5 add lineto
                  x2 y1 moveto
                  x2 3.5 sub y1 1.5 sub lineto
                  stroke
                  grestore end
               } bind def

               /harmonicsgrace {
                  5 dict begin gsave
                  /fret exch def
                  /y1 exch def
                  /x1 exch def
                  /x1 x1 2.5 sub store
                  /y1 y1 2.3 add store
                  /x2 x1 7.7 add store
                  fret 9 gt {
                     /x1 x1 0.9 sub store
                     /x2 x2 1.2 add store
                  }{} ifelse
                  0.4 setlinewidth
                  x1 y1 moveto
                  x1 2.5 add y1 1.1 add lineto
                  x1 y1 moveto
                  x1 2.5 add y1 1.1 sub lineto
                  x2 y1 moveto
                  x2 2.5 sub y1 1.1 add lineto
                  x2 y1 moveto
                  x2 2.5 sub y1 1.1 sub lineto
                  stroke
                  grestore end
               } bind def

               /pluckcurve {
                  2 dict begin gsave
                  /y1 exch def
                  /x1 exch def
                  0.1 setlinewidth
                  x1 y1 moveto
                  x1 1 sub y1 1 add x1 1 sub y1 1.5 add x1 y1 3 add curveto
                  x1 1.5 add y1 4 add x1 1.5 add y1 4.5 add x1 y1 6 add curveto
                  x1 0.15 sub y1 6.1 add x1 0.4 sub y1 5.85 add x1 0.3 sub y1 5.7 add curveto
                  x1 0.1 add y1 5.2 add x1 0.1 add y1 4.8 add x1 0.5 sub y1 4 add curveto
                  x1 2.3 sub y1 2.3 add x1 2.3 sub y1 2.2 add x1 0.3 sub y1 0.3 sub curveto
                  x1 0.15 sub y1 0.4 sub x1 0.1 add y1 0.15 sub x1 y1 curveto
                  fill
                  grestore end
               } bind def

               /pluckcurvegrace {
                  2 dict begin gsave
                  /y1 exch def
                  /x1 exch def
                  0.1 setlinewidth
                  x1 y1 moveto
                  x1 0.7 sub y1 1 add x1 0.7 sub y1 1.5 add x1 y1 3 add curveto
                  x1 1.3 add y1 4 add x1 1.3 add y1 4.5 add x1 y1 6 add curveto
                  x1 0.1 sub y1 6.1 add x1 0.3 sub y1 5.85 add x1 0.2 sub y1 5.7 add curveto
                  x1 0.07 add y1 5.2 add x1 0.07 add y1 4.8 add x1 0.3 sub y1 4 add curveto
                  x1 1.6 sub y1 2.3 add x1 1.6 sub y1 2.2 add x1 0.2 sub y1 0.3 sub curveto
                  x1 0.1 sub y1 0.4 sub x1 0.07 add y1 0.15 sub x1 y1 curveto
                  fill
                  grestore end
               } bind def

               /arrowup {
                  2 dict begin gsave
                  /y1 exch def
                  /x1 exch def
                  0.1 setlinewidth
                  x1 2 add y1 moveto
                  x1 y1 4 add lineto
                  x1 2 sub y1 lineto
                  x1 2 add y1 lineto
                  fill
                  grestore end
               } bind def

               /arrowupgrace {
                  2 dict begin gsave
                  /y1 exch def
                  /x1 exch def
                  0.1 setlinewidth
                  x1 1.5 add y1 moveto
                  x1 y1 3 add lineto
                  x1 1.5 sub y1 lineto
                  x1 1.5 add y1 lineto
                  fill
                  grestore end
               } bind def

               /pluckup {
                  4 dict begin gsave
                  /start1 exch def
                  /height1 exch def
                  /y1 exch def
                  /x1 exch def
                  /x1 x1 2 sub store
                  /y1 y1 3.5 sub start1 1 sub 6 mul add store
                  1 1 height1 {
                     /num exch def
                     x1 num 1 sub 6 mul y1 add pluckcurve
                  } for
                  x1 height1 6 mul y1 add 0.3 sub arrowup
                  grestore end
               } bind def

               /pluckupgrace {
                  4 dict begin gsave
                  /start1 exch def
                  /height1 exch def
                  /y1 exch def
                  /x1 exch def
                  /x1 x1 2 sub store
                  /y1 y1 3.5 sub start1 1 sub 6 mul add store
                  1 1 height1 {
                     /num exch def
                     x1 num 1 sub 6 mul y1 add pluckcurvegrace
                  } for
                  x1 height1 6 mul y1 add 0.3 sub arrowupgrace
                  grestore end
               } bind def

               /arrowdown {
                  2 dict begin gsave
                  /y1 exch def
                  /x1 exch def
                  0.1 setlinewidth
                  x1 2 add y1 moveto
                  x1 y1 4 sub lineto
                  x1 2 sub y1 lineto
                  x1 2 add y1 lineto
                  fill
                  grestore end
               } bind def

               /arrowdowngrace {
                  2 dict begin gsave
                  /y1 exch def
                  /x1 exch def
                  0.1 setlinewidth
                  x1 1.5 add y1 moveto
                  x1 y1 3 sub lineto
                  x1 1.5 sub y1 lineto
                  x1 1.5 add y1 lineto
                  fill
                  grestore end
               } bind def

               /pluckdown {
                  4 dict begin gsave
                  /start1 exch def
                  /height1 exch def
                  /y1 exch def
                  /x1 exch def
                  /x1 x1 2 sub store
                  /y1 y1 3 sub start1 1 sub 6 mul add store
                  1 1 height1 {
                     /num exch def
                     x1 num 1 sub -6 mul y1 add pluckcurve
                  } for
                  x1 height1 1 sub -6 mul y1 add arrowdown
                  grestore end
               } bind def

               /pluckdowngrace {
                  4 dict begin gsave
                  /start1 exch def
                  /height1 exch def
                  /y1 exch def
                  /x1 exch def
                  /x1 x1 2 sub store
                  /y1 y1 3.5 sub start1 1 sub 6 mul add store
                  1 1 height1 {
                     /num exch def
                     x1 num 1 sub -6 mul y1 add pluckcurvegrace
                  } for
                  x1 height1 1 sub -6 mul y1 add arrowdowngrace
                  grestore end
               } bind def

               /tupletBracket {
                  5 dict begin gsave
                  /num exch def
                  /y2 exch def
                  /x2 exch def
                  /y1 exch def
                  /x1 exch def
                  /mid x2 x1 sub 0.5 mul x1 add def
                  0.4 setlinewidth
                  x1 1 sub y1 46 add moveto
                  x1 1 sub y1 48 add lineto
                  mid 0.5 sub y1 48 add lineto
                  stroke

                  /mid2 mid 0.5 sub def
                  num 9 gt {
                     /mid mid 1.5 add store
                     /mid2 mid2 1 sub store
                  }{} ifelse

                  /Times-Roman findfont
                  5 scalefont setfont
                  newpath
                  0 0 0 setrgbcolor
                  mid2 1 add y1 47 add moveto
                  /s 20 string def
                  num s cvs show

                  mid 4 add y1 48 add moveto
                  x2 4.5 add y2 48 add lineto
                  x2 4.5 add y2 46 add lineto
                  stroke
                  grestore end
               } bind def

               /changeCapo {
                  3 dict begin gsave
                  /str exch def
                  /y1 exch def
                  /x1 exch def

                  /Times-Roman findfont
                  7 scalefont
                  setfont
                  newpath
                  0 0 0 setrgbcolor
                  x1 y1 55 add moveto
                  str show
                  grestore end
               } bind def

               /comment {
               3 dict begin gsave
               /str exch def
               /y1 exch def
               /x1 exch def
               /Times-Roman findfont
               7 scalefont
               setfont
               newpath
               0 0 0 setrgbcolor
               x1 2 add y1 50 add moveto
               str stringwidth pop -0.5 mul 0 rmoveto
               str show
               grestore end
               } bind def

               /hammer {
                  6 dict begin gsave
                  0.6 setlinewidth
                  /fret2 exch def
                  /fret1 exch def
                  /y2 exch def
                  /x2 exch def
                  /y1 exch def
                  /x1 exch def
                  fret1 9 gt {
                     /x1 x1 1.5 add store
                  }{} ifelse
                  fret2 9 gt {
                     /x2 x2 1.5 sub store
                  }{} ifelse
                 /x1 x1 4 add store
                 /x2 x2 0 sub store
                 /y2 y2 3.7 add store
                 /y1 y1 3.7 add store
                 x1 y1 moveto
                 /temp x2 x1 sub 0.3 mul def
                 x1 temp add y1 1.3 add x2 temp sub y2 1.3 add
                 x2 y2 curveto
                 stroke
                  grestore end
               } bind def

               /hammerbothgrace {
                  6 dict begin gsave
                  0.4 setlinewidth
                  /fret2 exch def
                  /fret1 exch def
                  /y2 exch def
                  /x2 exch def
                  /y1 exch def
                  /x1 exch def
                  fret1 9 gt {
                     /x1 x1 1 add store
                  }{} ifelse
                  fret2 9 gt {
                     /x2 x2 1 sub store
                  }{} ifelse
                 /x1 x1 2.8 add store
                 /x2 x2 0 sub store
                 /y2 y2 3.7 add store
                 /y1 y1 3.7 add store
                 x1 y1 moveto
                 /temp x2 x1 sub 0.3 mul def
                 x1 temp add y1 1.3 add x2 temp sub y2 1.3 add
                 x2 y2 curveto
                 stroke
                  grestore end
               } bind def

               /hammerfirstgrace {
                  6 dict begin gsave
                  0.4 setlinewidth
                  /fret2 exch def
                  /fret1 exch def
                  /y2 exch def
                  /x2 exch def
                  /y1 exch def
                  /x1 exch def
                  fret1 9 gt {
                     /x1 x1 1 add store
                  }{} ifelse
                  fret2 9 gt {
                     /x2 x2 1.5 sub store
                  }{} ifelse
                 /x1 x1 2.8 add store
                 /x2 x2 0 sub store
                 /y2 y2 3.7 add store
                 /y1 y1 3.7 add store
                 x1 y1 moveto
                 /temp x2 x1 sub 0.3 mul def
                 x1 temp add y1 1.3 add x2 temp sub y2 1.3 add
                 x2 y2 curveto
                 stroke
                  grestore end
               } bind def

               /hammersecondgrace {
                  6 dict begin gsave
                  0.4 setlinewidth
                  /fret2 exch def
                  /fret1 exch def
                  /y2 exch def
                  /x2 exch def
                  /y1 exch def
                  /x1 exch def
                  fret1 9 gt {
                     /x1 x1 1.5 add store
                  }{} ifelse
                  fret2 9 gt {
                     /x2 x2 1 sub store
                  }{} ifelse
                 /x1 x1 4 add store
                 /x2 x2 0 sub store
                 /y2 y2 3.7 add store
                 /y1 y1 3.7 add store
                 x1 y1 moveto
                 /temp x2 x1 sub 0.3 mul def
                 x1 temp add y1 1.3 add x2 temp sub y2 1.3 add
                 x2 y2 curveto
                 stroke
                  grestore end
               } bind def

               /palmmute {
               3 dict begin gsave
               /str (PM) def
               /y1 exch def
               /x1 exch def
               /Times-Roman findfont
               7 scalefont
               setfont
               newpath
               0 0 0 setrgbcolor
               x1 2 add y1 10 sub moveto
               str stringwidth pop -0.5 mul 0 rmoveto
               str show
               grestore end
               } bind def

               /longpalmmute {
               6 dict begin gsave
               /y2 exch def
               /x2 exch def
               /y1 exch def
               /x1 exch def
               x1 y1 palmmute
               /y1 y1 7.8 sub store
               /x2 x2 2 add store
               /x1 x1 8 add store
               0.5 setlinewidth
               /counter x2 x1 sub 6 div floor cvi def
               /current 0 def
               counter {
                  current 6 mul x1 add y1 moveto
                  /current current 1 add store
                  4 0 rlineto
                  stroke
               } repeat
               /nextStart current 6 mul x1 add def
               nextStart y1 moveto
               x2 nextStart sub 4 le {
                  x2 y1 lineto
                  stroke
               }{
                  4 0 rlineto
                  stroke
               } ifelse
               x2 y1 moveto
               0 3 rlineto stroke
               grestore end
               } bind def

               %%EndProlog

               "
               // Add the title and composer to the text
               let text' = text + " (" + newOption.Title + ") title " + "(" + newOption.Composer + ") composer (capo " + string newOption.Capo + ") capo " + " (Tuning: " + newOption.Tuning + ") tuning "
               //print and show
               match (show pages [] text' "" outFile newOption.TuningNumbers) with
               | Some(updatedText, updatedPages) ->

                  Some(updatedText, updatedPages)
               | None -> None
            | None -> None
         | None -> None
      | None -> None
   | None ->None
































///
