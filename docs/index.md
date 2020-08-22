# Blueberry

With a simple syntax, blueberry lets you focus on the music, and not on annoying UIs!

&nbsp;

![Example](img/blueberry-example.png)

&nbsp;

* TOC
{:toc}

&nbsp;

# Usage

In order to run the program, you need to have [.NET](https://dotnet.microsoft.com/) and [Ghostscript](https://www.ghostscript.com/) installed. Follow the instructions for downloading and setup.

## Mac

Download the packages above and clone this repo into an easily accessible location.

### Running Within the Blueberry Folder

1. The quickest way to run the program is directly in the cloned repo. Open terminal, and navigate to the lang folder within blueberry.

   ```shell
   cd path-to-folder/blueberry/lang
   ```

2. Create a new file to write your code in. I suggest using the .blb extension for easy-of-use, but any extension works.

3. Run using dotnet. It will create a .ps file with the specified name.

   ```shell
   dotnet run <filename> <output filename>
   ```

4. Use ps2pdf, a command line tool from Ghostscript, to convert the postscript file to a pdf. If you get an error, you may need to add ghostscript to your PATH variable.
   ```shell
   ps2pdf -dNOSAFER <file>.ps
   ```
### Running From Separate Folder

1. To run the program on files in a separate folder, I suggest writing a custom script to do the job for you. Follow [this link](https://medium.com/devnetwork/how-to-create-your-own-custom-terminal-commands-c5008782a78e) for instructions on how to set up a file with custom scripts. Once created, add the following, replacing PATHTOFILE with the path to wherever you cloned the blueberry folder:
   ```shell
   function pspdf() {
      ps2pdf -dNOSAFER $1.ps
      open $1.pdf
   }

   function blb() {
      if [[ "$#" == 0 ]]; then
      echo 'Usage: blb <file with code> <name of output file>'
      return 0
      fi
      if [ -z "$2" ]
         then
            OUTFILE='score'
      else
         OUTFILE=$2
      fi
      cwd=$(pwd)
      cd ~/PATHTOFILE/blueberry/lang
      dotnet run $cwd/$1 $OUTFILE
      pspdf $OUTFILE
      rm $OUTFILE.ps
      mv $OUTFILE.pdf $cwd
      cd $cwd
   }
   ```
Now, you can create files anywhere on your computer, and run it using `blb <filename> <outputFile>`!

## Windows

Download the packages above and clone this repo into an easily accessible location. In addition, make sure you have [Virtual Studio Code](https://code.visualstudio.com/).

The instructions for running within the blueberry folder should be the same as for Mac users. Use the terminal function within VsCode. In addition, check out this [post](https://stackoverflow.com/questions/6011373/call-ghostscript-in-windows-by-its-invocation-name#:~:text=Open%20a%20cmd%20window%20and,64%20bit%20Windows) for how to use the ghostscript functions in the terminal. I'm not familiar with scripting on Windows and how to create a script to run the program from other folders.

# Documentation

## Basic Syntax

   ```   
   -title Example
   -composer Person
   -key c
   -time 4-4
   -capo 1

   1:
      1f4
      3g#
      5d8/sli
      5f8
      4e4
   2:
      2e16/sls
      2f16
      2g
      2a/sle
      6bb2
      1f8/gra
      2a4
   ```

![General setup](img/general.png)

Each file begins with options that control certain aspects of the tab. They begin with a `-`, followed by the option name, and the chosen parameter. Next, each measure begins with the measure number, a colon, and the notes on separate lines with a tab (or at least a space).

## Options

Here are the valid options:

   ```
   -title Blueberry
   -composer Hugo Hua
   -key d#m
   -time 4-4
   -capo 2
   ```

The title and composer can be whatever you choose, and the capo is an integer.

### Key

Valid keys are:

* c
* cm
* c#
* c#m
* cb
* d
* dm
* db
* d#m
* e
* em
* eb
* ebm
* f
* fm
* f#m
* f#
* g
* gm
* g#m
* gb
* a
* am
* a#m
* ab
* abm
* b
* bm
* bb
* bbm

Notes that are entered will be automatically changed based on the key chosen.

### Time

Time signatures are of the format `<top>-<bottom>`. The first number can be a number between 1-32, or 64, and the second number can be 1, 2, 4, 8, 16, 32, or 64.

## Notes and Rests

There are several basic note types: single notes, rests, groups, and tuplets.

Single notes follow the format `<string number><pitch><OPTIONAL rhythm><OPTIONAL properties>`. Here are some examples:

   ```
   1e4
   5f#/gra
   6x8../gra/slu
   ```

Rests follow the format `r<OPTIONAL rhythm>`

   ```
   r
   r8
   r16..
   ```

Grouped notes allow for multiple notes to be played at the same time. They follow the format `(<note>+)<OPTIONAL rhythm><OPTIONAL group-properties>` where `<note>` is composed of `<string number><pitch><OPTIONAL group-member-properties>`. I know, looks wild, but it's not that bad! Properties work differently with grouped notes, because some properties must be applied to an individual note within the group, while others are applied to the entire group. See the [properties](https://blueberry.hugohua.com/#properties) section for more info.

   ```
   (1e 2e 3e)4
   (4f#/har 6dn)16/gra
   (3dn 4fb 6x/par)
   ```

Tuplets allow for more complex rhythms and syncopation. They follow the format `<<note>+><rhythm>`, where `<note>` is a single note, group note, or rest. The rhythm at the end denotes the total rhythm of the entire tuplet. Note that the final rhythm is _required_ for tuplets.

   ```
   <4d8 2f16>4
   <6d r 1f (5f 3d)>8
   <1f#8/har 2dn4/par/sli/sls (4f#/sld 1x/har)16./gra/sle>32.
   ```

### Pitch

The following are valid pitches:

* c
* c#
* cb
* cn
* d
* d#
* db
* dn
* e
* e#
* eb
* en
* f
* f#
* fb
* fn
* g
* g#
* gb
* gn
* a
* a#
* ab
* an
* b
* b#
* bb
* bn
* x

Notes with an `n` are natural notes, which differ from regular notes in that the key signature will not change them. The `x` represents a percussive note rather than a pitch; it shows up as an "X" rather than a fret number. Using the pitches, key signature, and capo, the program calculates the fret for that note on the given string. The default is the lowest possible fret, but there is a property that can raise frets, which will be explained in the [properties](https://blueberry.hugohua.com/#properties) section.

### Rhythm

Rhythms are denoted with a number after the pitch.

- 1 = whole note
- 2 = half note
- 4 = quarter note
- 8 = eighth note
- 16 = sixteenth note
- 32 = 32nd note
- 64 = 64th note

Dots `.` can also be added after the number. The limit is 3 dots for notes of an 8th note or longer, 2 dots on a 16th note, 1 dot on a 32nd note, and no dots are allowed on a 64th note. Rhythms are optional; if omitted, the note will have the same rhythm as the previous note. The default for the first note on the page is a quarter note.

## Properties



## Welcome to GitHub Pages

You can use the [editor on GitHub](https://github.com/hkh4/hkh4.github.io/edit/master/README.md) to maintain and preview the content for your website in Markdown files.

Whenever you commit to this repository, GitHub Pages will run [Jekyll](https://jekyllrb.com/) to rebuild the pages in your site, from the content in your Markdown files.

### Markdown

Markdown is a lightweight and easy-to-use syntax for styling your writing. It includes conventions for

```markdown
Syntax highlighted code block

# Header 1
## Header 2
### Header 3

- Bulleted
- List

1. Numbered
2. List

**Bold** and _Italic_ and `Code` text

[Link](url) and ![Image](src)
```

For more details see [GitHub Flavored Markdown](https://guides.github.com/features/mastering-markdown/).

### Jekyll Themes

Your Pages site will use the layout and styles from the Jekyll theme you have selected in your [repository settings](https://github.com/hkh4/hkh4.github.io/settings). The name of this theme is saved in the Jekyll `_config.yml` configuration file.

### Support or Contact

Having trouble with Pages? Check out our [documentation](https://help.github.com/categories/github-pages-basics/) or [contact support](https://github.com/contact) and we’ll help you sort it out.
