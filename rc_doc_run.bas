If NumCommands() < 2 Then
	End 3
End If

src_dir$ = Command$(1)
dst_dir$ = Command$(2)

If Not DirExists(dst_dir$) Then
	Print "Destination directory not found"
	End 1
End If

If Not DirExists(src_dir$) Then
	Print "Source directory not found"
	End 2
End If

If Trim(Left(src_dir$,1)) <> "/" Then
	src_dir$ = Dir$ + "/" + src_dir$
End If

If Trim(Left(dst_dir$,1)) <> "/" Then
	dst_dir$ = Dir$ + "/" + dst_dir$
	'Print "DST: "; dst_dir$
End If

prg_dir$ = Dir$
ChangeDir(src_dir$)
dir_file$ = DirFirst$

Dim src_file$[9999]
Dim src_file_count : src_file_count = 0

While Trim(dir_file$) <> ""
	'Print "SRC: "; dir_file$
	If FileExists(dir_file$) And Trim$(Left$(dir_file$, 1)) <> "." Then
		src_file$[src_file_count] = dir_file$
		src_file_count = src_file_count + 1
	End If
	dir_file$ = DirNext$
Wend

cmd_dir$ = Reverse(Command$(0))

For i = 0 to Len(cmd_dir$)-1
	If Mid(cmd_dir$, i, 1) = "/" Then
		cmd_dir$ = Mid(cmd_dir$, i, Len(cmd_dir$))
		Exit For
	End If
Next

cmd_dir$ = Reverse(cmd_dir$)
cmd$ = Trim$(Env$("RC_DOC_HOME"))

If Right(cmd$, 1) <> "/" Then
	cmd$ = cmd$ + "/rc_doc"
End If

ChangeDir(dst_dir$)

For i = 0 to src_file_count-1
	Print "rcbasic " + cmd$ + " " + src_dir$+"/"+src_file$[i] + " ./" + Replace$(Trim$(LCase$(src_file$[i])),".txt",".html")
	If FileExists(src_dir$+"/"+src_file$[i]) And Trim$(Right$(LCase$(src_file$[i]), 4)) = ".txt" Then
		System("rcbasic " + cmd$ + " " + src_dir$+"/"+src_file$[i] + " ./" + Replace$(Trim$(LCase$(src_file$[i])),".txt",".html"))
	End If
Next

Print "-Doc Generation Complete-"

End 0