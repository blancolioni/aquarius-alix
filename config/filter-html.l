; -*- scheme -*-

(aquarius-filter
 (filter-name "html")
 (header "<html>"
	 "<head>"
	 "<title>Aquarius</title>"
	 "<meta name=generator content=\"Aquarius ${version}\""
	 "</head>"
	 "<body>"
	 "<pre>")
 
 (footer "</pre>"
	"</body>")

 (keyword "<strong>" "@@" "</strong>")
 (line-comment "<font color=""red""><i>" "@@" "</i></font>")
 (string "<font color=""rosybrown"">" "@@" "</font>")
 (string-literal "<font color=""rosybrown"">" "@@" "</font>")
 (type-indication "<font color=""forestgreen"">" "@@" "</font>")
 (type-header "<font color=""forestgreen"">" "@@" "</font>")
 (procedure-name "<font color=""blue"">" "@@" "</font>")
 (function-name "<font color=""blue"">" "@@" "</font>")
 (block-name "<font color=""blue"">" "@@" "</font>")
 (package-name "<font color=""blue"">" "@@" "</font>")
 (external-unit-name "<font color=""cadetblue"">" "@@" "</font>")
 (error "<font color=""red""><b>" "@@" "</b></font>")
 )
