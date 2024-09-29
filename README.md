# RCDocs
A tool to assist in writing documentation

## Building RCDocs
To build RCDocs, just build rc_docs.bas with RCBasic.

`rcbasic_build rc_docs.bas`

To generate html docs from your text files you need to either pass a directory or a file to RCDocs.

`rcbasic rc_docs name_of_text_file.txt`


## Writing RCDocs file
RCDocs are structured with a tile, a header, and the contents of the document. Here is an example:

```
 \#title My Document Title
 \#header My Document Header
 
 A paragraph is started by typing something on a line.
 
 Another paragraph is created if a new non-blank line is made after an empty line.
 
 \#image "test.png"
 A paragraph is also created if the last line was a command line.
```

RCDocs consist of paragraphs and commands. Here are all the available commands:
- \#image "image_file", OPTIONAL( w=image_width, h=image_height )
- \#table - tables can only have row commands in them.
    - #row "column 1", "column 2", "etc."
- \#/table - this ends the table
- \#list (ul / ol) - Passing the ul argument creates an unordered list and ol creates an ordered list
- \#li contents
- \#/list
- \#code - starts a code block
- \#/code - ends a code block
- \#video "video_file", OPTIONAL( w=image_width, h=image_height )
- \#ref doc1, doc2, etc. - Adds links to related docs. The args passed to this must not have file extensions. Just the name of the doc.

Refer to the test_doc.txt example for how each command should look.
