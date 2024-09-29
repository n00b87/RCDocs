' RCDocs - Documentation tool
'-------------------------------------------
' Author: n00b aka Rodney Cunningham
' Website: http:://rcbasic.freeforums.net
'-------------------------------------------

Dim doc_base_src$ : doc_base_src$ = ""

Dim doc_title$

Dim doc_header$

Dim table_style$, body_style$, number_style$, string_style$, keyword_style$, comment_style$, cblock_style$
table_style$ = "table, th, td { border: 1px solid black; } th, td { padding: 10px; }"
body_style$ = "body { background-color: #A0A0FF; }"
number_style$ = ".rc_number { color: #800000; }"
keyword_style$ = ".rc_keyword { color: blue; font-weight: bold; }"
string_style$ = ".rc_string { color: green; }"
comment_style$ = ".rc_comment { color: #0A0A0A; }"
cblock_style$ = "#rc_code { font-family: Consolas,\qcourier new\q; background-color: #f1f1f1; padding: 2px; font-size: 105%; }"

Dim doc_tokens$[9999]
Dim num_doc_tokens

Dim delim$[9999]
Dim num_delim

Dim doc_cmd$[9999,30]
Dim num_doc_cmd
Dim current_doc_cmd
Dim num_doc_cmd_args[9999]

Dim par_flag
Dim set_flag
Dim cblock_flag
Dim table_flag
Dim table_row_flag
Dim list_flag
Dim list_item_flag
Dim current_list
Dim txt_flag

Dim cblock$[999]

Dim export_source$[9999]
Dim num_export_source : num_export_source = 0


Dim list_stack : list_stack = 0
Dim list_item_p_stack : list_item_p_stack = 1

Dim tmp_id$

Dim par_txt$

Dim error_flag
Dim error$

Sub OutputDoc(output_file$)
	doc_base_src$ = Replace(doc_base_src$, "[title]", doc_title$)

	style_tab$ = "\t\t\t"
	style_src$ = ""
	style_src$ = style_src$ + style_tab$ + table_style$ + "\n"
	style_src$ = style_src$ + style_tab$ + body_style$ + "\n"
	style_src$ = style_src$ + style_tab$ + number_style$ + "\n"
	style_src$ = style_src$ + style_tab$ + string_style$ + "\n"
	style_src$ = style_src$ + style_tab$ + keyword_style$ + "\n"
	style_src$ = style_src$ + style_tab$ + comment_style$ + "\n"
	style_src$ = style_src$ + style_tab$ + cblock_style$ + "\n"
	doc_base_src$ = Replace(doc_base_src$, "[style]", Trim(style_src$))
	
	body_src$ = "\t\t<p>" + doc_header$ + "</p>\n"
	For i = 0 to num_export_source-1
		body_src$ = body_src$ + export_source$[i] + "\n"
	Next
	
	doc_base_src$ = Replace(doc_base_src$, "[body]", body_src$)
	
	FileOpen(0, output_file$, TEXT_OUTPUT)
	Write(0, doc_base_src$)
	FileClose(0)
	
End Sub

Function isDelim( dval$ )
	dval$ = Trim(dval$)
	For i = 0 to num_delim-1
		If dval$ = delim$[i] Then
			Return True
		End If
	Next
	
	Return False
End Function

Sub addDelim( delim_val$ )
	If (num_delim + 1) >= ArraySize(delim$, 1) Then
		ReDim delim$[num_delim+100]
	End If
	
	delim$[num_delim] = delim_val$
	num_delim = num_delim + 1
End Sub

Function AnyFlag()
	Return par_flag Or set_flag Or cblock_flag
End Function

Sub SetFlags( t$ )
	t$ = Trim(t$)
	
	If num_doc_tokens = 0 and Not AnyFlag() Then
		If t$ <> "" Then
			If Left$(t, 1) <> "#" Then
				par_flag = True
			End If
		Else
			par_flag = False
		End If
	End If
	
	If t$ = "=" Then
		set_flag = True
	End If
	
	
	Select Case t$
	Case "#row", "#ref"
		table_row_flag = True
	Case "#h", "#header", "#list", "#title"
		txt_flag = True
	Case "#li"
		If current_list >= 0 Then
			txt_flag = True
			doc_cmd$[current_list, 2] = "li_init"
		Else
			error$ = "Can't have list item outside list"
			error_flag = True
		End If
	End Select
	
End Sub

Sub ExpandID(ByRef s_id$)
	s_id$ = Trim(s_id$)
	Select Case s_id$
	Case "w": s_id$ = "width"
	Case "h": s_id$ = "height"
	End Select
End Sub

Sub addToken( doc_token_val$ )
	If Replace$(doc_token_val$, " ", "") = "" Then
		Return
	End If
	
	If (num_doc_tokens + 1) >= ArraySize(doc_tokens$, 1) Then
		ReDim doc_tokens$[num_doc_tokens+200]
	End If
	
	If set_flag Then
		If isDelim(doc_token_val$) Or Left(doc_token_val$, 1)="#" Then
			error_flag = True
			error$ = "Illegal value for argument"
			Return
		Else
			ExpandID(tmp_id$)
			doc_cmd$[current_doc_cmd, num_doc_cmd_args[current_doc_cmd]+1] = tmp_id$ + "=" + Trim(doc_token_val$)
			num_doc_cmd_args[current_doc_cmd] = num_doc_cmd_args[current_doc_cmd] + 1
			set_flag = False
			tmp_id$ = ""
		End If
	End If
	
	SetFlags( doc_token_val$ )
	
	If Left(doc_token_val$, 1) <> "#" Then
		If Not isDelim(doc_token_val$) Then
			tmp_id$ = doc_token_val$
		End If

		If table_row_flag Or (Left(doc_token_val$, 1) = "\q") Then
			doc_cmd$[current_doc_cmd, num_doc_cmd_args[current_doc_cmd]+1] = Trim(doc_token_val$)
			num_doc_cmd_args[current_doc_cmd] = num_doc_cmd_args[current_doc_cmd] + 1
		End If
		
	Else
		current_doc_cmd = num_doc_cmd
		num_doc_cmd = num_doc_cmd + 1
		doc_cmd$[current_doc_cmd, 0] = Trim(doc_token_val$)
		num_doc_cmd_args[current_doc_cmd] = 0
	End If
	
	doc_tokens$[num_doc_tokens] = doc_token_val$
	num_doc_tokens = num_doc_tokens + 1
End Sub

Sub init_doc()
	num_doc_tokens = 0
	num_delim = 0
	num_doc_cmd = 0
	num_doc_tokens = 0
	num_export_source = 0
	
	ArrayFill(export_source$, "")
	ArrayFill(doc_cmd$, "")
	ArrayFill(num_doc_cmd_args, 0)
	
	Stack_N(list_stack)
	For i = 0 to Stack_Size_N()-1
		Pop_N()
	Next
	
	Stack_N(list_item_p_stack)
	For i = 0 to Stack_Size_N()-1
		Pop_N()
	Next
	
	par_flag = 0
	set_flag = 0
	cblock_flag = 0
	table_flag = 0
	table_row_flag = 0
	list_flag = 0
	list_item_flag = 0
	txt_flag = 0
	error_flag = 0
	
	error$ = ""
	tmp_id$ = ""
	
	current_list = -1
	
	addDelim(" ")
	addDelim("(")
	addDelim(",")
	addDelim("[")
	addDelim("=")
	addDelim(")")
	addDelim("]")
	
	doc_base_src$ = doc_base_src$ + "<!DOCTYPE html>\n"
	doc_base_src$ = doc_base_src$ + "<html>\n"
	doc_base_src$ = doc_base_src$ + "\t<head>\n"
	doc_base_src$ = doc_base_src$ + "\t\t<style>\n"
	doc_base_src$ = doc_base_src$ + "[style]\n"
	doc_base_src$ = doc_base_src$ + "\t\t</style>\n"
	doc_base_src$ = doc_base_src$ + "\t\t<meta  content=\qtext/html; charset=UTF-8\q  http-equiv=\qcontent-type\q>\n"
	doc_base_src$ = doc_base_src$ + "\t\t<title>[title]</title>\n"
	doc_base_src$ = doc_base_src$ + "\t</head>\n\n"
	doc_base_src$ = doc_base_src$ + "\t<body>\n"
	doc_base_src$ = doc_base_src$ + "[body]\n"
	doc_base_src$ = doc_base_src$ + "\t</body>\n"
	doc_base_src$ = doc_base_src$ + "</html>"
	
	doc_header$ = "<h1>[arg]</h1>"
	doc_title$ = "<title>[arg]</title>"
End Sub

Dim rc_keyword$[99]
Dim num_keywords : num_keywords = 0
Dim rc_functions$[9999]
Dim num_functions : num_functions = 0

rc_keyword$[0] = "print"
rc_keyword$[1] = "for"
rc_keyword$[2] = "to"
rc_keyword$[3] = "next"
rc_keyword$[4] = "if"
rc_keyword$[5] = "then"
rc_keyword$[6] = "else"
rc_keyword$[7] = "elseif"
rc_keyword$[8] = "end"
rc_keyword$[9] = "while"
rc_keyword$[10] = "wend"
rc_keyword$[11] = "do"
rc_keyword$[12] = "loop"
rc_keyword$[13] = "until"
rc_keyword$[14] = "dim"
rc_keyword$[15] = "redim"
rc_keyword$[16] = "as"
rc_keyword$[17] = "type"
rc_keyword$[18] = "function"
rc_keyword$[19] = "sub"
rc_keyword$[20] = "mod"
rc_keyword$[21] = "and"
rc_keyword$[22] = "or"
rc_keyword$[23] = "xor"
rc_keyword$[24] = "not"
rc_keyword$[25] = "select"
rc_keyword$[26] = "case"
rc_keyword$[27] = "return"
rc_keyword$[28] = "true"
rc_keyword$[29] = "false"

num_keywords = 30


Function rc_getExpandedCBID$(txt$)
	txt$ = Trim(txt$)
	l_txt$ = LCase(txt$)
	For i = 0 to num_keywords-1
		If l_txt$ = rc_keyword$[i] Then
			Return "<span class=\qrc_keyword\q>" + txt$ + "</span>"
		End If
	Next
	Return txt$
End Function

Sub addCodeBlockLine(src$)
	src$ = Trim(src$) + " "
	cb_token$ = ""
	
	cb_comment$ = ""
	
	cb_scope = 0
	
	quote = 0
	
	cb_parse$ = ""
	
	For i = 0 to Len(src$)-1
		c$ = Mid(src$, i, 1)
		If c$ = "\q" Then
			If quote Then
				cb_scope = cb_scope - 1
				quote = False
				cb_parse$ = cb_parse$ + "\q</span>"
			Else
				cb_scope = cb_scope + 1
				quote = True
				cb_parse$ = cb_parse$ + "<span class=\qrc_string\q>\q"
			End If
			cb_parse$ = cb_parse$ + rc_getExpandedCBID(cb_token$)
			Continue
		End If
		
		If Not quote Then
			Select Case c$
			Case "(", ")", "[", "]", ",", ";", ":", " ", "+", "-", "/", "*", "^", "'"
				cb_token$ = rc_getExpandedCBID(cb_token$)
				cb_parse$ = cb_parse$ + cb_token$
				cb_token$ = ""
			End Select
		
			Select Case c$
			Case "(", "["
				cb_scope = cb_scope + 1
				cb_parse$ = cb_parse$ + "<b>" + c$ + "</b>"
			Case ")", "]"
				cb_scope = cb_scope - 1
				cb_parse$ = cb_parse$ + "<b>" + c$ + "</b>"
			Case ">"
				cb_parse$ = cb_parse$ + "&gt;"
			Case "<"
				cb_parse$ = cb_parse$ + "&lt;"
			Case " "
				cb_parse$ = cb_parse$ + "&nbsp;"
			Case "\t"
				cb_parse$ = cb_parse$ + "&nbsp;&nbsp;&nbsp;" 
			Case "'"
				If cb_scope = 0 Then
					cb_parse$ = cb_parse$ + "<span class=\qrc_comment\q>" + Mid(src$, i, Len(src$)) + "</span>"
					Exit For
				End If
			Case " ", "\t"
				cb_parse$ = cb_parse$ + c$
			Case "0", "1", "2", "3", "4", "5", "6", "7", "8", "9"
				cb_token$ = "<span class=\qrc_number\q>"
				For i = i to Len(src$)-1
					Select Case Mid(src$, i, 1)
					Case "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "."
						cb_token$ = cb_token$ + Mid(src$, i, 1)
					Default
						cb_token$ = cb_token$ + "</span>"
						i = i - 1
						cb_parse$ = cb_parse$ + cb_token$
						cb_token$ = ""
						
						Exit For
					End Select
				Next
			Default
				cb_token$ = cb_token$ + c$
			End Select
		Else
			cb_parse$ = cb_parse$ + c$
		End If
		
	Next

	export_source$[num_export_source] = "\t\t\t" + cb_parse$ + "<br>"
	num_export_source = num_export_source + 1
End Sub

Sub addExportSource(src$)
	export_source$[num_export_source] = "\t\t" + src$
	num_export_source = num_export_source + 1
End Sub

Function str_format$( txt$ )
	txt$ = Trim(txt$)
	If Left(txt$, 1) = "\q" Then
		txt$ = Mid$(txt$, 1, Len(txt$))
		If Len(txt$) > 1 Then
			txt$ = Mid(txt$, 0, Len(txt$)-1)
		End If
	End If
	Return txt$
End Function

Sub RemoveLastExportSource()
	If num_export_source > 0 Then
		num_export_source = num_export_source - 1
		export_source$[num_export_source] = ""
	End If
End Sub

Function tokenize_line( doc_line$ )
	pos = 0
	continue_loop = True
	current_token$ = ""
	doc_line$ = doc_line$ + " "
	tmp_id$ = ""
	
	If cblock_flag Then
		If Left(doc_line$, Len("#/code")) = "#/code" Then
			addExportSource("</code></p>")
			cblock_flag = False
			Return True
		Else
			addCodeBlockLine(Trim(doc_line$))
			Return True
		End If
	End If
	
	If par_flag Then
		If Left(doc_line$, 1) = "#" Or Trim(doc_line$) = " " Or Trim(doc_line$) = "" Then
			par_flag = False
			If Replace(Trim(par_txt$), " ", "") = "" Then
				removeLastExportSource()
			Else
				addExportSource(par_txt$)
				addExportSource("</p>")
			End If
		Else
			par_txt$ = par_txt$ + doc_line$
			Return True
		End If
	End If
	
		
	
	If Not (table_flag Or list_flag) Then
		current_doc_cmd = 0
		num_doc_cmd = 0
		
		If Left(doc_line$, 1) <> "#" And (Not par_flag) Then
			par_flag = True
			addExportSource("<p>")
			par_txt$ = doc_line$
			Return True
		End If
		
	End If
	
	start_cmd = num_doc_cmd
	
	str_flag = 0
	
	while continue_loop
		If txt_flag Then
			doc_cmd$[start_cmd, 1] = Mid(doc_line$, pos, Len(doc_line$))
			num_doc_cmd_args[start_cmd] = 1
			Exit While
		End If
		
		If pos >= Len(doc_line$) Then
			Exit While
		End If
		c$ = Mid$(doc_line$, pos, 1)
		If c$ = "\q" Then
			current_token$ = current_token$ + c$
			str_flag = Not str_flag
			If Not str_flag Then
				addToken(current_token$)
				current_token$ = ""
			End If
		ElseIf isDelim(c$) And (Not str_flag) Then
			addToken(current_token$)
			addToken(c$)
			current_token$ = ""
		Else
			'Print "CT = ";c$
			current_token$ = current_token$ + c$
		End If
		pos = pos + 1
		If error_flag Then
			Return False
		End If
	Wend
	
	txt_flag = False
	
	final_tokens$ = ""
	For i = 0 to num_doc_tokens-1
		final_tokens$ = final_tokens$ + "token[" + str(i) + "]=" + doc_tokens$[i] + "\n"
	Next
	
	If table_flag And LCase(doc_cmd$[start_cmd,0]) <> "#row" And LCase(doc_cmd$[start_cmd,0]) <> "#/table" Then
		Print "RC_DOC tables only support text content"
		Return False
	End If
	
	Select Case LCase(doc_cmd$[start_cmd,0])
	Case "#video"
		video_tag$ = "<p><video [args] controls>"
		source_tag$ = "<source src=\q[src]\q type=\q[vtype]\q>"
		video_src$ = ""
		video_args$ = ""
		video_type$ = ""
		For i = 1 To num_doc_cmd_args[start_cmd]
			If InStr(doc_cmd$[start_cmd, i], "=") < 0 Then
				video_src$ = Trim( Replace(doc_cmd$[start_cmd, i], "\q", "") )
				video_type$ = "video/" + LCase(Right(Trim(Replace(doc_cmd$[start_cmd, i], "\q", "")), 3))
			Else
				video_args$ = video_args$ + Trim(doc_cmd$[start_cmd, i]) + " "
			End If
		Next
		
		video_tag$ = Replace(video_tag$, "[args]", video_args$)
		source_tag$ = Replace(source_tag$, "[src]", Trim(video_src$))
		source_tag$ = Replace(source_tag$, "[vtype]", Trim(video_type$))
		
		addExportSource(video_tag$)
		addExportSource(source_tag$)
		addExportSource("Browser does not support playing video")
		addExportSource("</video></p>")
		
	Case "#image"
		img_tag$ = "<p><img src=\q[src]\q [args]></p>"
		img_src$ = ""
		img_args$ = ""
		For i = 1 To num_doc_cmd_args[start_cmd]
			If InStr(doc_cmd$[start_cmd, i], "=") < 0 Then
				img_src$ = Trim( Replace(doc_cmd$[start_cmd, i], "\q", "") )
			Else
				img_args$ = img_args$ + Trim(doc_cmd$[start_cmd, i]) + " "
			End If
		Next
		
		img_tag$ = Replace(img_tag$, "[src]", img_src$)
		img_tag$ = Replace(img_tag$, "[args]", Trim(img_args$))
		addExportSource(img_tag$)
	Case "#table"
		table_flag = True
	Case "#/table"
		table_flag = False
		num_cols = 0
		
		For i = 0 to start_cmd-1
			num_cols = Max(num_cols, num_doc_cmd_args[i])
		Next
		
		addExportSource("<table>")
		
		row_src$ = ""
		For i = 1 to start_cmd-1
			row_src$ = "<tr>"
			For col = 1 to num_cols
				If col > num_doc_cmd_args[i] Then
					row_src$ = row_src$ + Replace("<th>[args]</th>", "[args]", " ")
				ElseIf doc_cmd$[i, col] <> "," Then
					doc_cmd$[i, col] = str_format(doc_cmd$[i, col])
					row_src$ = row_src$ + Replace("<th>[args]</th>", "[args]", doc_cmd$[i, col])
				End If
			Next
			row_src$ = row_src$ + "</tr>"
			addExportSource(row_src$)
			row_src$ = ""
		Next
		
		ArrayFill(doc_cmd$, "")
		
		addExportSource("</table>")
	Case "#row"
		table_row_flag = False
	Case "#header", "#h"
		doc_header$ = "<h1>"
		For i = 1 to num_doc_cmd_args[start_cmd]
			doc_header$ = doc_header$ + doc_cmd$[start_cmd, i]
		Next
		doc_header$ = doc_header$ + "</h1>"	
	Case "#title"
		doc_title$ = ""
		For i = 1 to num_doc_cmd_args[start_cmd]
			doc_title$ = doc_title$ + doc_cmd$[start_cmd, i]
		Next
		doc_title$ = doc_title$
		
	Case "#list"
		Stack_N(list_item_p_stack)
		Push_N(list_item_flag)
		list_item_flag = False
		
		list_flag = list_flag + 1
		
		'print "test list: "; doc_cmd$[start_cmd, 1]; " -- "; num_doc_cmd_args[start_cmd]
		
		doc_cmd$[start_cmd, 2] = ""
		
		doc_cmd$[start_cmd, 1] = Trim(doc_cmd$[start_cmd, 1])
		
		Select Case doc_cmd$[start_cmd, 1]
		Case "ul"
			addExportSource("<ul>")
		Case "ol"
			addExportSource("<ol>")
		Case ""
			doc_cmd$[start_cmd, 1] = "ul"
			addExportSource("<ul>")
		Default
				error$ = "Invalid list type (" + doc_cmd$[start_cmd, 1] + ")"
				error_flag = True
				Return False
		End Select
		
		Stack_N(list_stack)
		Push_N(start_cmd)
		
		current_list = start_cmd
	Case "#/list"
		If list_flag > 0 Then
			list_flag = list_flag - 1
		Else
			error$ = "No list to close"
			error_flag = True
			Return False
		End If
		
		If list_item_flag Then
			addExportSource("</li>")
			list_item_flag = False
		End If
		
		Stack_N(list_stack)
		closing_list = Pop_N()
		
		Select Case doc_cmd$[closing_list, 1]
		Case "ul"
			addExportSource("</ul>")
		Case "ol"
			addExportSource("</ol>")
		Default
			error$ = "No list type"
			error_flag = True
			Return False
		End Select
		
		If Stack_Size_N() > 0 Then
			Stack_N(list_stack)
			current_list = Pop_N()
			Push_N(current_list)
			
			Stack_N(list_item_p_stack)
			list_item_flag = Pop_N()
			
			If doc_cmd$[current_list, 2] = "li_init" Then
				list_item_flag = True
			Else
				list_item_flag = False
			End If
		Else
			current_list = -1
		End If
	
	Case "#li"
		If current_list < 0 Then
			error$ = "Can't add list item outside list"
			error_flag = True
			Return False
		End If
		
		If list_item_flag Then
			addExportSource("</li>")
		End If
		
		If doc_cmd$[current_list, 2] = "li_init" Then
			list_item_flag = True
		End If
		addExportSource("<li>")
		For i = 1 To num_doc_cmd_args[start_cmd]
			addExportSource(doc_cmd$[start_cmd, i])
		Next
		
	Case "#code"
		cblock_flag = True
		addExportSource("<p id=\qrc_code\q><code>")
		
	Case "#ref"
		'Print "ref found"
		addExportSource("<br><p>Related: ")
		For i = 1 to num_doc_cmd_args[start_cmd]
			addExportSource("<a href=\q" + LCase$(Trim(Replace(doc_cmd$[start_cmd, i],"$",""))) + ".html\q>" + doc_cmd$[start_cmd, i] + "</a>")
		Next
		addExportSource("</p>")
		table_row_flag = False
	End Select
		
	
	Return True
End Function

Function ParseFile(src_file$)
	init_doc
		
	If Not FileExists(src_file$) Then
		Return False
	End If
	
	FileOpen(0, src_file$, TEXT_INPUT)
	
	While Not EOF(0)
		If Not tokenize_line(ReadLine(0)) Then
			FileClose(0)
			Return False
		End If
	Wend
	
	FileClose(0)
	Return True
		
End Function

If NumCommands() > 1 Then
	If DirExists(Command$(1)) Then
		ChangeDir(Command$(1))
		src_file$ = Trim(DirFirst$)
		While src_file$ <> ""
			If Right$(src_file$, 4) = ".txt" Then
				Print "Parse "; src_file$; ": ";
				If ParseFile(src_file$) Then
					Print "SUCCESS"
					OutputDoc(Replace(src_file$, ".txt", ".html"))
				Else
					Print "FAIL: "; error$
				End If
			End If
			src_file$ = DirNext$()
		Wend
		
		Print "All Docs Generated"
	ElseIf FileExists(Command$(1)) Then
		src_file$ = Trim(Command$(1))
		If Right$(src_file$, 4) <> ".txt" Then
			Print "Must provide *.txt File"
			End 0
		End If
		out_file$ = Replace(src_file$, ".txt", ".html")
		
		If ParseFile(src_file$) Then
			OutputDoc(out_file$)
			Print "Generated output file: "; out_file$
		Else
			Print "---Failed to generate Doc: "; error$
		End If
	End If
End If

End
