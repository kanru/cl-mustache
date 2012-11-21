;;;; Auto-generated from mustache spec
(in-package :mustache-test)

(deftest spec
 (plan 122)
 (is
  (mustache-render-to-string "12345 {{! Comment Block! }} 67890"
   (mustache-context :data 'nil :partials 'nil))
  "12345  67890"
  (format nil "~A :: ~A" "Surrounding Whitespace"
          "Comment removal should preserve surrounding whitespace."))
 (is
  (mustache-render-to-string "  12 {{! 34 }}
"
   (mustache-context :data 'nil :partials 'nil))
  "  12 
"
  (format nil "~A :: ~A" "Indented Inline"
          "Inline comments should not strip whitespace"))
 (is
  (mustache-render-to-string "Begin.
  {{!
    Something's going on here...
  }}
End.
"
   (mustache-context :data 'nil :partials 'nil))
  "Begin.
End.
"
  (format nil "~A :: ~A" "Indented Multiline Standalone"
          "All standalone comment lines should be removed."))
 (is
  (mustache-render-to-string "Begin.
{{!
Something's going on here...
}}
End.
"
   (mustache-context :data 'nil :partials 'nil))
  "Begin.
End.
"
  (format nil "~A :: ~A" "Multiline Standalone"
          "All standalone comment lines should be removed."))
 (is
  (mustache-render-to-string "!
  {{! I'm Still Standalone }}"
   (mustache-context :data 'nil :partials 'nil))
  "!
"
  (format nil "~A :: ~A" "Standalone Without Newline"
          "Standalone tags should not require a newline to follow them."))
 (is
  (mustache-render-to-string "  {{! I'm Still Standalone }}
!"
   (mustache-context :data 'nil :partials 'nil))
  "!"
  (format nil "~A :: ~A" "Standalone Without Previous Line"
          "Standalone tags should not require a newline to precede them."))
 (is
  (mustache-render-to-string "|
{{! Standalone Comment }}
|"
   (mustache-context :data 'nil :partials 'nil))
  "|
|"
  (format nil "~A :: ~A" "Standalone Line Endings"
          "\"\\r\\n\" should be considered a newline for standalone tags."))
 (is
  (mustache-render-to-string "Begin.
  {{! Indented Comment Block! }}
End.
"
   (mustache-context :data 'nil :partials 'nil))
  "Begin.
End.
"
  (format nil "~A :: ~A" "Indented Standalone"
          "All standalone comment lines should be removed."))
 (is
  (mustache-render-to-string "Begin.
{{! Comment Block! }}
End.
"
   (mustache-context :data 'nil :partials 'nil))
  "Begin.
End.
"
  (format nil "~A :: ~A" "Standalone"
          "All standalone comment lines should be removed."))
 (is
  (mustache-render-to-string "12345{{!
  This is a
  multi-line comment...
}}67890
"
   (mustache-context :data 'nil :partials 'nil))
  "1234567890
"
  (format nil "~A :: ~A" "Multiline"
          "Multiline comments should be permitted."))
 (is
  (mustache-render-to-string "12345{{! Comment Block! }}67890"
   (mustache-context :data 'nil :partials 'nil))
  "1234567890"
  (format nil "~A :: ~A" "Inline"
          "Comment blocks should be removed from the template."))
 (is
  (mustache-render-to-string "|{{= @   @ =}}|"
   (mustache-context :data 'nil :partials 'nil))
  "||"
  (format nil "~A :: ~A" "Pair with Padding"
          "Superfluous in-tag whitespace should be ignored."))
 (is
  (mustache-render-to-string "=
  {{=@ @=}}"
   (mustache-context :data 'nil :partials 'nil))
  "=
"
  (format nil "~A :: ~A" "Standalone Without Newline"
          "Standalone tags should not require a newline to follow them."))
 (is
  (mustache-render-to-string "  {{=@ @=}}
="
   (mustache-context :data 'nil :partials 'nil))
  "="
  (format nil "~A :: ~A" "Standalone Without Previous Line"
          "Standalone tags should not require a newline to precede them."))
 (is
  (mustache-render-to-string "|
{{= @ @ =}}
|"
   (mustache-context :data 'nil :partials 'nil))
  "|
|"
  (format nil "~A :: ~A" "Standalone Line Endings"
          "\"\\r\\n\" should be considered a newline for standalone tags."))
 (is
  (mustache-render-to-string "Begin.
  {{=@ @=}}
End.
"
   (mustache-context :data 'nil :partials 'nil))
  "Begin.
End.
"
  (format nil "~A :: ~A" "Indented Standalone Tag"
          "Indented standalone lines should be removed from the template."))
 (is
  (mustache-render-to-string "Begin.
{{=@ @=}}
End.
"
   (mustache-context :data 'nil :partials 'nil))
  "Begin.
End.
"
  (format nil "~A :: ~A" "Standalone Tag"
          "Standalone lines should be removed from the template."))
 (is
  (mustache-render-to-string " | {{=@ @=}}
"
   (mustache-context :data 'nil :partials 'nil))
  " | 
"
  (format nil "~A :: ~A" "Outlying Whitespace (Inline)"
          "Whitespace should be left untouched."))
 (is
  (mustache-render-to-string "| {{=@ @=}} |"
   (mustache-context :data 'nil :partials 'nil))
  "|  |"
  (format nil "~A :: ~A" "Surrounding Whitespace"
          "Surrounding whitespace should be left untouched."))
 (is
  (mustache-render-to-string "[ {{>include}} ]
[ .{{value}}.  .|value|. ]
"
   (mustache-context :data '((:value . "yes")) :partials
    '((:include . ".{{value}}. {{= | | =}} .|value|."))))
  "[ .yes.  .yes. ]
[ .yes.  .|value|. ]
"
  (format nil "~A :: ~A" "Post-Partial Behavior"
          "Delimiters set in a partial should not affect the parent template."))
 (is
  (mustache-render-to-string "[ {{>include}} ]
{{= | | =}}
[ |>include| ]
"
   (mustache-context :data '((:value . "yes")) :partials
    '((:include . ".{{value}}."))))
  "[ .yes. ]
[ .yes. ]
"
  (format nil "~A :: ~A" "Partial Inheritence"
          "Delimiters set in a parent template should not affect a partial."))
 (is
  (mustache-render-to-string "[
{{^section}}
  {{data}}
  |data|
{{/section}}

{{= | | =}}
|^section|
  {{data}}
  |data|
|/section|
]
"
   (mustache-context :data '((:section) (:data . "I got interpolated."))
    :partials 'nil))
  "[
  I got interpolated.
  |data|

  {{data}}
  I got interpolated.
]
"
  (format nil "~A :: ~A" "Inverted Sections"
          "Delimiters set outside inverted sections should persist."))
 (is
  (mustache-render-to-string "[
{{#section}}
  {{data}}
  |data|
{{/section}}

{{= | | =}}
|#section|
  {{data}}
  |data|
|/section|
]
"
   (mustache-context :data '((:section . t) (:data . "I got interpolated."))
    :partials 'nil))
  "[
  I got interpolated.
  |data|

  {{data}}
  I got interpolated.
]
"
  (format nil "~A :: ~A" "Sections"
          "Delimiters set outside sections should persist."))
 (is
  (mustache-render-to-string "({{=[ ]=}}[text])"
   (mustache-context :data '((:text . "It worked!")) :partials 'nil))
  "(It worked!)"
  (format nil "~A :: ~A" "Special Characters"
          "Characters with special meaning regexen should be valid delimiters."))
 (is
  (mustache-render-to-string "{{=<% %>=}}(<%text%>)"
   (mustache-context :data '((:text . "Hey!")) :partials 'nil))
  "(Hey!)"
  (format nil "~A :: ~A" "Pair Behavior"
          "The equals sign (used on both sides) should permit delimiter changes."))
 (is
  (mustache-render-to-string "|{{& string }}|"
   (mustache-context :data '((:string . "---")) :partials 'nil))
  "|---|"
  (format nil "~A :: ~A" "Ampersand With Padding"
          "Superfluous in-tag whitespace should be ignored."))
 (is
  (mustache-render-to-string "|{{{ string }}}|"
   (mustache-context :data '((:string . "---")) :partials 'nil))
  "|---|"
  (format nil "~A :: ~A" "Triple Mustache With Padding"
          "Superfluous in-tag whitespace should be ignored."))
 (is
  (mustache-render-to-string "|{{ string }}|"
   (mustache-context :data '((:string . "---")) :partials 'nil))
  "|---|"
  (format nil "~A :: ~A" "Interpolation With Padding"
          "Superfluous in-tag whitespace should be ignored."))
 (is
  (mustache-render-to-string "  {{&string}}
"
   (mustache-context :data '((:string . "---")) :partials 'nil))
  "  ---
"
  (format nil "~A :: ~A" "Ampersand - Standalone"
          "Standalone interpolation should not alter surrounding whitespace."))
 (is
  (mustache-render-to-string "  {{{string}}}
"
   (mustache-context :data '((:string . "---")) :partials 'nil))
  "  ---
"
  (format nil "~A :: ~A" "Triple Mustache - Standalone"
          "Standalone interpolation should not alter surrounding whitespace."))
 (is
  (mustache-render-to-string "  {{string}}
"
   (mustache-context :data '((:string . "---")) :partials 'nil))
  "  ---
"
  (format nil "~A :: ~A" "Interpolation - Standalone"
          "Standalone interpolation should not alter surrounding whitespace."))
 (is
  (mustache-render-to-string "| {{&string}} |"
   (mustache-context :data '((:string . "---")) :partials 'nil))
  "| --- |"
  (format nil "~A :: ~A" "Ampersand - Surrounding Whitespace"
          "Interpolation should not alter surrounding whitespace."))
 (is
  (mustache-render-to-string "| {{{string}}} |"
   (mustache-context :data '((:string . "---")) :partials 'nil))
  "| --- |"
  (format nil "~A :: ~A" "Triple Mustache - Surrounding Whitespace"
          "Interpolation should not alter surrounding whitespace."))
 (is
  (mustache-render-to-string "| {{string}} |"
   (mustache-context :data '((:string . "---")) :partials 'nil))
  "| --- |"
  (format nil "~A :: ~A" "Interpolation - Surrounding Whitespace"
          "Interpolation should not alter surrounding whitespace."))
 (is
  (mustache-render-to-string "\"{{#a}}{{b.c.d.e.name}}{{/a}}\" == \"Phil\""
   (mustache-context :data
    '((:a (:b (:c (:d (:e (:name . "Phil"))))))
      (:b (:c (:d (:e (:name . "Wrong"))))))
    :partials 'nil))
  "\"Phil\" == \"Phil\""
  (format nil "~A :: ~A" "Dotted Names - Initial Resolution"
          "The first part of a dotted name should resolve as any other name."))
 (is
  (mustache-render-to-string "\"{{a.b.c.name}}\" == \"\""
   (mustache-context :data '((:a (:b)) (:c (:name . "Jim"))) :partials 'nil))
  "\"\" == \"\""
  (format nil "~A :: ~A" "Dotted Names - Broken Chain Resolution"
          "Each part of a dotted name should resolve only against its parent."))
 (is
  (mustache-render-to-string "\"{{a.b.c}}\" == \"\""
   (mustache-context :data '((:a)) :partials 'nil))
  "\"\" == \"\""
  (format nil "~A :: ~A" "Dotted Names - Broken Chains"
          "Any falsey value prior to the last part of the name should yield ''."))
 (is
  (mustache-render-to-string "\"{{a.b.c.d.e.name}}\" == \"Phil\""
   (mustache-context :data '((:a (:b (:c (:d (:e (:name . "Phil")))))))
    :partials 'nil))
  "\"Phil\" == \"Phil\""
  (format nil "~A :: ~A" "Dotted Names - Arbitrary Depth"
          "Dotted names should be functional to any level of nesting."))
 (is
  (mustache-render-to-string
   "\"{{&person.name}}\" == \"{{#person}}{{&name}}{{/person}}\""
   (mustache-context :data '((:person (:name . "Joe"))) :partials 'nil))
  "\"Joe\" == \"Joe\""
  (format nil "~A :: ~A" "Dotted Names - Ampersand Interpolation"
          "Dotted names should be considered a form of shorthand for sections."))
 (is
  (mustache-render-to-string
   "\"{{{person.name}}}\" == \"{{#person}}{{{name}}}{{/person}}\""
   (mustache-context :data '((:person (:name . "Joe"))) :partials 'nil))
  "\"Joe\" == \"Joe\""
  (format nil "~A :: ~A" "Dotted Names - Triple Mustache Interpolation"
          "Dotted names should be considered a form of shorthand for sections."))
 (is
  (mustache-render-to-string
   "\"{{person.name}}\" == \"{{#person}}{{name}}{{/person}}\""
   (mustache-context :data '((:person (:name . "Joe"))) :partials 'nil))
  "\"Joe\" == \"Joe\""
  (format nil "~A :: ~A" "Dotted Names - Basic Interpolation"
          "Dotted names should be considered a form of shorthand for sections."))
 (is
  (mustache-render-to-string "I ({{&cannot}}) be seen!"
   (mustache-context :data 'nil :partials 'nil))
  "I () be seen!"
  (format nil "~A :: ~A" "Ampersand Context Miss Interpolation"
          "Failed context lookups should default to empty strings."))
 (is
  (mustache-render-to-string "I ({{{cannot}}}) be seen!"
   (mustache-context :data 'nil :partials 'nil))
  "I () be seen!"
  (format nil "~A :: ~A" "Triple Mustache Context Miss Interpolation"
          "Failed context lookups should default to empty strings."))
 (is
  (mustache-render-to-string "I ({{cannot}}) be seen!"
   (mustache-context :data 'nil :partials 'nil))
  "I () be seen!"
  (format nil "~A :: ~A" "Basic Context Miss Interpolation"
          "Failed context lookups should default to empty strings."))
 (is
  (mustache-render-to-string "\"{{&power}} jiggawatts!\""
   (mustache-context :data '((:power . 1.21)) :partials 'nil))
  "\"1.21 jiggawatts!\""
  (format nil "~A :: ~A" "Ampersand Decimal Interpolation"
          "Decimals should interpolate seamlessly with proper significance."))
 (is
  (mustache-render-to-string "\"{{{power}}} jiggawatts!\""
   (mustache-context :data '((:power . 1.21)) :partials 'nil))
  "\"1.21 jiggawatts!\""
  (format nil "~A :: ~A" "Triple Mustache Decimal Interpolation"
          "Decimals should interpolate seamlessly with proper significance."))
 (is
  (mustache-render-to-string "\"{{power}} jiggawatts!\""
   (mustache-context :data '((:power . 1.21)) :partials 'nil))
  "\"1.21 jiggawatts!\""
  (format nil "~A :: ~A" "Basic Decimal Interpolation"
          "Decimals should interpolate seamlessly with proper significance."))
 (is
  (mustache-render-to-string "\"{{&mph}} miles an hour!\""
   (mustache-context :data '((:mph . 85)) :partials 'nil))
  "\"85 miles an hour!\""
  (format nil "~A :: ~A" "Ampersand Integer Interpolation"
          "Integers should interpolate seamlessly."))
 (is
  (mustache-render-to-string "\"{{{mph}}} miles an hour!\""
   (mustache-context :data '((:mph . 85)) :partials 'nil))
  "\"85 miles an hour!\""
  (format nil "~A :: ~A" "Triple Mustache Integer Interpolation"
          "Integers should interpolate seamlessly."))
 (is
  (mustache-render-to-string "\"{{mph}} miles an hour!\""
   (mustache-context :data '((:mph . 85)) :partials 'nil))
  "\"85 miles an hour!\""
  (format nil "~A :: ~A" "Basic Integer Interpolation"
          "Integers should interpolate seamlessly."))
 (is
  (mustache-render-to-string
   "These characters should not be HTML escaped: {{&forbidden}}
"
   (mustache-context :data '((:forbidden . "& \" < >")) :partials 'nil))
  "These characters should not be HTML escaped: & \" < >
"
  (format nil "~A :: ~A" "Ampersand"
          "Ampersand should interpolate without HTML escaping."))
 (is
  (mustache-render-to-string
   "These characters should not be HTML escaped: {{{forbidden}}}
"
   (mustache-context :data '((:forbidden . "& \" < >")) :partials 'nil))
  "These characters should not be HTML escaped: & \" < >
"
  (format nil "~A :: ~A" "Triple Mustache"
          "Triple mustaches should interpolate without HTML escaping."))
 (is
  (mustache-render-to-string
   "These characters should be HTML escaped: {{forbidden}}
"
   (mustache-context :data '((:forbidden . "& \" < >")) :partials 'nil))
  "These characters should be HTML escaped: &amp; &quot; &lt; &gt;
"
  (format nil "~A :: ~A" "HTML Escaping"
          "Basic interpolation should be HTML escaped."))
 (is
  (mustache-render-to-string "Hello, {{subject}}!
"
   (mustache-context :data '((:subject . "world")) :partials 'nil))
  "Hello, world!
"
  (format nil "~A :: ~A" "Basic Interpolation"
          "Unadorned tags should interpolate content into the template."))
 (is
  (mustache-render-to-string "Hello from {Mustache}!
"
   (mustache-context :data 'nil :partials 'nil))
  "Hello from {Mustache}!
"
  (format nil "~A :: ~A" "No Interpolation"
          "Mustache-free templates should render as-is."))
 (is
  (mustache-render-to-string "|{{^ boolean }}={{/ boolean }}|"
   (mustache-context :data '((:boolean)) :partials 'nil))
  "|=|"
  (format nil "~A :: ~A" "Padding"
          "Superfluous in-tag whitespace should be ignored."))
 (is
  (mustache-render-to-string "^{{^boolean}}
/
  {{/boolean}}"
   (mustache-context :data '((:boolean)) :partials 'nil))
  "^
/
"
  (format nil "~A :: ~A" "Standalone Without Newline"
          "Standalone tags should not require a newline to follow them."))
 (is
  (mustache-render-to-string "  {{^boolean}}
^{{/boolean}}
/"
   (mustache-context :data '((:boolean)) :partials 'nil))
  "^
/"
  (format nil "~A :: ~A" "Standalone Without Previous Line"
          "Standalone tags should not require a newline to precede them."))
 (is
  (mustache-render-to-string "|
{{^boolean}}
{{/boolean}}
|"
   (mustache-context :data '((:boolean)) :partials 'nil))
  "|
|"
  (format nil "~A :: ~A" "Standalone Line Endings"
          "\"\\r\\n\" should be considered a newline for standalone tags."))
 (is
  (mustache-render-to-string "| This Is
  {{^boolean}}
|
  {{/boolean}}
| A Line
"
   (mustache-context :data '((:boolean)) :partials 'nil))
  "| This Is
|
| A Line
"
  (format nil "~A :: ~A" "Standalone Indented Lines"
          "Standalone indented lines should be removed from the template."))
 (is
  (mustache-render-to-string "| This Is
{{^boolean}}
|
{{/boolean}}
| A Line
"
   (mustache-context :data '((:boolean)) :partials 'nil))
  "| This Is
|
| A Line
"
  (format nil "~A :: ~A" "Standalone Lines"
          "Standalone lines should be removed from the template."))
 (is
  (mustache-render-to-string " {{^boolean}}NO{{/boolean}}
 {{^boolean}}WAY{{/boolean}}
"
   (mustache-context :data '((:boolean)) :partials 'nil))
  " NO
 WAY
"
  (format nil "~A :: ~A" "Indented Inline Sections"
          "Single-line sections should not alter surrounding whitespace."))
 (is
  (mustache-render-to-string " | {{^boolean}} {{! Important Whitespace }}
 {{/boolean}} | 
"
   (mustache-context :data '((:boolean)) :partials 'nil))
  " |  
  | 
"
  (format nil "~A :: ~A" "Internal Whitespace"
          "Inverted should not alter internal whitespace."))
 (is
  (mustache-render-to-string " | {{^boolean}}	|	{{/boolean}} | 
"
   (mustache-context :data '((:boolean)) :partials 'nil))
  " | 	|	 | 
"
  (format nil "~A :: ~A" "Surrounding Whitespace"
          "Inverted sections should not alter surrounding whitespace."))
 (is
  (mustache-render-to-string "\"{{^a.b.c}}Not Here{{/a.b.c}}\" == \"Not Here\""
   (mustache-context :data '((:a)) :partials 'nil))
  "\"Not Here\" == \"Not Here\""
  (format nil "~A :: ~A" "Dotted Names - Broken Chains"
          "Dotted names that cannot be resolved should be considered falsey."))
 (is
  (mustache-render-to-string "\"{{^a.b.c}}Not Here{{/a.b.c}}\" == \"Not Here\""
   (mustache-context :data '((:a (:b (:c)))) :partials 'nil))
  "\"Not Here\" == \"Not Here\""
  (format nil "~A :: ~A" "Dotted Names - Falsey"
          "Dotted names should be valid for Inverted Section tags."))
 (is
  (mustache-render-to-string "\"{{^a.b.c}}Not Here{{/a.b.c}}\" == \"\""
   (mustache-context :data '((:a (:b (:c . t)))) :partials 'nil))
  "\"\" == \"\""
  (format nil "~A :: ~A" "Dotted Names - Truthy"
          "Dotted names should be valid for Inverted Section tags."))
 (is
  (mustache-render-to-string
   "[{{^missing}}Cannot find key 'missing'!{{/missing}}]"
   (mustache-context :data 'nil :partials 'nil))
  "[Cannot find key 'missing'!]"
  (format nil "~A :: ~A" "Context Misses"
          "Failed context lookups should be considered falsey."))
 (is
  (mustache-render-to-string
   "| A {{^bool}}B {{^bool}}C{{/bool}} D{{/bool}} E |"
   (mustache-context :data '((:bool . t)) :partials 'nil))
  "| A  E |"
  (format nil "~A :: ~A" "Nested (Truthy)"
          "Nested truthy sections should be omitted."))
 (is
  (mustache-render-to-string
   "| A {{^bool}}B {{^bool}}C{{/bool}} D{{/bool}} E |"
   (mustache-context :data '((:bool)) :partials 'nil))
  "| A B C D E |"
  (format nil "~A :: ~A" "Nested (Falsey)"
          "Nested falsey sections should have their contents rendered."))
 (is
  (mustache-render-to-string "{{^bool}}
* first
{{/bool}}
* {{two}}
{{^bool}}
* third
{{/bool}}
"
   (mustache-context :data '((:two . "second") (:bool)) :partials 'nil))
  "* first
* second
* third
"
  (format nil "~A :: ~A" "Doubled"
          "Multiple inverted sections per template should be permitted."))
 (is
  (mustache-render-to-string "\"{{^list}}Yay lists!{{/list}}\""
   (mustache-context :data '((:list . #())) :partials 'nil))
  "\"Yay lists!\""
  (format nil "~A :: ~A" "Empty List"
          "Empty lists should behave like falsey values."))
 (is
  (mustache-render-to-string "\"{{^list}}{{n}}{{/list}}\""
   (mustache-context :data '((:list . #(((:n . 1)) ((:n . 2)) ((:n . 3)))))
    :partials 'nil))
  "\"\""
  (format nil "~A :: ~A" "List" "Lists should behave like truthy values."))
 (is
  (mustache-render-to-string "\"{{^context}}Hi {{name}}.{{/context}}\""
   (mustache-context :data '((:context (:name . "Joe"))) :partials 'nil))
  "\"\""
  (format nil "~A :: ~A" "Context"
          "Objects and hashes should behave like truthy values."))
 (is
  (mustache-render-to-string
   "\"{{^boolean}}This should not be rendered.{{/boolean}}\""
   (mustache-context :data '((:boolean . t)) :partials 'nil))
  "\"\""
  (format nil "~A :: ~A" "Truthy"
          "Truthy sections should have their contents omitted."))
 (is
  (mustache-render-to-string
   "\"{{^boolean}}This should be rendered.{{/boolean}}\""
   (mustache-context :data '((:boolean)) :partials 'nil))
  "\"This should be rendered.\""
  (format nil "~A :: ~A" "Falsey"
          "Falsey sections should have their contents rendered."))
 (is
  (mustache-render-to-string "|{{> partial }}|"
   (mustache-context :data '((:boolean . t)) :partials '((:partial . "[]"))))
  "|[]|"
  (format nil "~A :: ~A" "Padding Whitespace"
          "Superfluous in-tag whitespace should be ignored."))
 (is
  (mustache-render-to-string "\\
 {{>partial}}
/
"
   (mustache-context :data
    '((:content . "<
->"))
    :partials
    '((:partial . "|
{{{content}}}
|
"))))
  "\\
 |
 <
->
 |
/
"
  (format nil "~A :: ~A" "Standalone Indentation"
          "Each line of the partial should be indented before rendering."))
 (is
  (mustache-render-to-string ">
  {{>partial}}"
   (mustache-context :data 'nil :partials
    '((:partial . ">
>"))))
  ">
  >
  >"
  (format nil "~A :: ~A" "Standalone Without Newline"
          "Standalone tags should not require a newline to follow them."))
 (is
  (mustache-render-to-string "  {{>partial}}
>"
   (mustache-context :data 'nil :partials
    '((:partial . ">
>"))))
  "  >
  >>"
  (format nil "~A :: ~A" "Standalone Without Previous Line"
          "Standalone tags should not require a newline to precede them."))
 (is
  (mustache-render-to-string "|
{{>partial}}
|"
   (mustache-context :data 'nil :partials '((:partial . ">"))))
  "|
>|"
  (format nil "~A :: ~A" "Standalone Line Endings"
          "\"\\r\\n\" should be considered a newline for standalone tags."))
 (is
  (mustache-render-to-string "  {{data}}  {{> partial}}
"
   (mustache-context :data '((:data . "|")) :partials
    '((:partial . ">
>"))))
  "  |  >
>
"
  (format nil "~A :: ~A" "Inline Indentation"
          "Whitespace should be left untouched."))
 (is
  (mustache-render-to-string "| {{>partial}} |"
   (mustache-context :data 'nil :partials '((:partial . "	|	"))))
  "| 	|	 |"
  (format nil "~A :: ~A" "Surrounding Whitespace"
          "The greater-than operator should not alter surrounding whitespace."))
 (is
  (mustache-render-to-string "{{>node}}"
   (mustache-context :data
    '((:content . "X") (:nodes . #(((:content . "Y") (:nodes . #())))))
    :partials '((:node . "{{content}}<{{#nodes}}{{>node}}{{/nodes}}>"))))
  "X<Y<>>"
  (format nil "~A :: ~A" "Recursion"
          "The greater-than operator should properly recurse."))
 (is
  (mustache-render-to-string "\"{{>partial}}\""
   (mustache-context :data '((:text . "content")) :partials
    '((:partial . "*{{text}}*"))))
  "\"*content*\""
  (format nil "~A :: ~A" "Context"
          "The greater-than operator should operate within the current context."))
 (is
  (mustache-render-to-string "\"{{>text}}\""
   (mustache-context :data 'nil :partials 'nil))
  "\"\""
  (format nil "~A :: ~A" "Failed Lookup"
          "The empty string should be used when the named partial is not found."))
 (is
  (mustache-render-to-string "\"{{>text}}\""
   (mustache-context :data 'nil :partials '((:text . "from partial"))))
  "\"from partial\""
  (format nil "~A :: ~A" "Basic Behavior"
          "The greater-than operator should expand to the named partial."))
 (is
  (mustache-render-to-string "|{{# boolean }}={{/ boolean }}|"
   (mustache-context :data '((:boolean . t)) :partials 'nil))
  "|=|"
  (format nil "~A :: ~A" "Padding"
          "Superfluous in-tag whitespace should be ignored."))
 (is
  (mustache-render-to-string "#{{#boolean}}
/
  {{/boolean}}"
   (mustache-context :data '((:boolean . t)) :partials 'nil))
  "#
/
"
  (format nil "~A :: ~A" "Standalone Without Newline"
          "Standalone tags should not require a newline to follow them."))
 (is
  (mustache-render-to-string "  {{#boolean}}
#{{/boolean}}
/"
   (mustache-context :data '((:boolean . t)) :partials 'nil))
  "#
/"
  (format nil "~A :: ~A" "Standalone Without Previous Line"
          "Standalone tags should not require a newline to precede them."))
 (is
  (mustache-render-to-string "|
{{#boolean}}
{{/boolean}}
|"
   (mustache-context :data '((:boolean . t)) :partials 'nil))
  "|
|"
  (format nil "~A :: ~A" "Standalone Line Endings"
          "\"\\r\\n\" should be considered a newline for standalone tags."))
 (is
  (mustache-render-to-string "| This Is
  {{#boolean}}
|
  {{/boolean}}
| A Line
"
   (mustache-context :data '((:boolean . t)) :partials 'nil))
  "| This Is
|
| A Line
"
  (format nil "~A :: ~A" "Indented Standalone Lines"
          "Indented standalone lines should be removed from the template."))
 (is
  (mustache-render-to-string "| This Is
{{#boolean}}
|
{{/boolean}}
| A Line
"
   (mustache-context :data '((:boolean . t)) :partials 'nil))
  "| This Is
|
| A Line
"
  (format nil "~A :: ~A" "Standalone Lines"
          "Standalone lines should be removed from the template."))
 (is
  (mustache-render-to-string " {{#boolean}}YES{{/boolean}}
 {{#boolean}}GOOD{{/boolean}}
"
   (mustache-context :data '((:boolean . t)) :partials 'nil))
  " YES
 GOOD
"
  (format nil "~A :: ~A" "Indented Inline Sections"
          "Single-line sections should not alter surrounding whitespace."))
 (is
  (mustache-render-to-string " | {{#boolean}} {{! Important Whitespace }}
 {{/boolean}} | 
"
   (mustache-context :data '((:boolean . t)) :partials 'nil))
  " |  
  | 
"
  (format nil "~A :: ~A" "Internal Whitespace"
          "Sections should not alter internal whitespace."))
 (is
  (mustache-render-to-string " | {{#boolean}}	|	{{/boolean}} | 
"
   (mustache-context :data '((:boolean . t)) :partials 'nil))
  " | 	|	 | 
"
  (format nil "~A :: ~A" "Surrounding Whitespace"
          "Sections should not alter surrounding whitespace."))
 (is
  (mustache-render-to-string "\"{{#a.b.c}}Here{{/a.b.c}}\" == \"\""
   (mustache-context :data '((:a)) :partials 'nil))
  "\"\" == \"\""
  (format nil "~A :: ~A" "Dotted Names - Broken Chains"
          "Dotted names that cannot be resolved should be considered falsey."))
 (is
  (mustache-render-to-string "\"{{#a.b.c}}Here{{/a.b.c}}\" == \"\""
   (mustache-context :data '((:a (:b (:c)))) :partials 'nil))
  "\"\" == \"\""
  (format nil "~A :: ~A" "Dotted Names - Falsey"
          "Dotted names should be valid for Section tags."))
 (is
  (mustache-render-to-string "\"{{#a.b.c}}Here{{/a.b.c}}\" == \"Here\""
   (mustache-context :data '((:a (:b (:c . t)))) :partials 'nil))
  "\"Here\" == \"Here\""
  (format nil "~A :: ~A" "Dotted Names - Truthy"
          "Dotted names should be valid for Section tags."))
 (is
  (mustache-render-to-string "\"{{#list}}({{.}}){{/list}}\""
   (mustache-context :data '((:list . #(1.1 2.2 3.3 4.4 5.5))) :partials 'nil))
  "\"(1.1)(2.2)(3.3)(4.4)(5.5)\""
  (format nil "~A :: ~A" "Implicit Iterator - Decimal"
          "Implicit iterators should cast decimals to strings and interpolate."))
 (is
  (mustache-render-to-string "\"{{#list}}({{.}}){{/list}}\""
   (mustache-context :data '((:list . #(1 2 3 4 5))) :partials 'nil))
  "\"(1)(2)(3)(4)(5)\""
  (format nil "~A :: ~A" "Implicit Iterator - Integer"
          "Implicit iterators should cast integers to strings and interpolate."))
 (is
  (mustache-render-to-string "\"{{#list}}({{.}}){{/list}}\""
   (mustache-context :data '((:list . #("a" "b" "c" "d" "e"))) :partials 'nil))
  "\"(a)(b)(c)(d)(e)\""
  (format nil "~A :: ~A" "Implicit Iterator - String"
          "Implicit iterators should directly interpolate strings."))
 (is
  (mustache-render-to-string "[{{#missing}}Found key 'missing'!{{/missing}}]"
   (mustache-context :data 'nil :partials 'nil))
  "[]"
  (format nil "~A :: ~A" "Context Misses"
          "Failed context lookups should be considered falsey."))
 (is
  (mustache-render-to-string
   "| A {{#bool}}B {{#bool}}C{{/bool}} D{{/bool}} E |"
   (mustache-context :data '((:bool)) :partials 'nil))
  "| A  E |"
  (format nil "~A :: ~A" "Nested (Falsey)"
          "Nested falsey sections should be omitted."))
 (is
  (mustache-render-to-string
   "| A {{#bool}}B {{#bool}}C{{/bool}} D{{/bool}} E |"
   (mustache-context :data '((:bool . t)) :partials 'nil))
  "| A B C D E |"
  (format nil "~A :: ~A" "Nested (Truthy)"
          "Nested truthy sections should have their contents rendered."))
 (is
  (mustache-render-to-string "{{#bool}}
* first
{{/bool}}
* {{two}}
{{#bool}}
* third
{{/bool}}
"
   (mustache-context :data '((:two . "second") (:bool . t)) :partials 'nil))
  "* first
* second
* third
"
  (format nil "~A :: ~A" "Doubled"
          "Multiple sections per template should be permitted."))
 (is
  (mustache-render-to-string "\"{{#list}}Yay lists!{{/list}}\""
   (mustache-context :data '((:list . #())) :partials 'nil))
  "\"\""
  (format nil "~A :: ~A" "Empty List"
          "Empty lists should behave like falsey values."))
 (is
  (mustache-render-to-string "\"{{#list}}{{item}}{{/list}}\""
   (mustache-context :data
    '((:list . #(((:item . 1)) ((:item . 2)) ((:item . 3))))) :partials 'nil))
  "\"123\""
  (format nil "~A :: ~A" "List"
          "Lists should be iterated; list items should visit the context stack."))
 (is
  (mustache-render-to-string "{{#a}}
{{one}}
{{#b}}
{{one}}{{two}}{{one}}
{{#c}}
{{one}}{{two}}{{three}}{{two}}{{one}}
{{#d}}
{{one}}{{two}}{{three}}{{four}}{{three}}{{two}}{{one}}
{{#e}}
{{one}}{{two}}{{three}}{{four}}{{five}}{{four}}{{three}}{{two}}{{one}}
{{/e}}
{{one}}{{two}}{{three}}{{four}}{{three}}{{two}}{{one}}
{{/d}}
{{one}}{{two}}{{three}}{{two}}{{one}}
{{/c}}
{{one}}{{two}}{{one}}
{{/b}}
{{one}}
{{/a}}
"
   (mustache-context :data
    '((:a (:one . 1)) (:b (:two . 2)) (:c (:three . 3)) (:d (:four . 4))
      (:e (:five . 5)))
    :partials 'nil))
  "1
121
12321
1234321
123454321
1234321
12321
121
1
"
  (format nil "~A :: ~A" "Deeply Nested Contexts"
          "All elements on the context stack should be accessible."))
 (is
  (mustache-render-to-string "\"{{#context}}Hi {{name}}.{{/context}}\""
   (mustache-context :data '((:context (:name . "Joe"))) :partials 'nil))
  "\"Hi Joe.\""
  (format nil "~A :: ~A" "Context"
          "Objects and hashes should be pushed onto the context stack."))
 (is
  (mustache-render-to-string
   "\"{{#boolean}}This should not be rendered.{{/boolean}}\""
   (mustache-context :data '((:boolean)) :partials 'nil))
  "\"\""
  (format nil "~A :: ~A" "Falsey"
          "Falsey sections should have their contents omitted."))
 (is
  (mustache-render-to-string
   "\"{{#boolean}}This should be rendered.{{/boolean}}\""
   (mustache-context :data '((:boolean . t)) :partials 'nil))
  "\"This should be rendered.\""
  (format nil "~A :: ~A" "Truthy"
          "Truthy sections should have their contents rendered."))
 (is
  (mustache-render-to-string "<{{^lambda}}{{static}}{{/lambda}}>"
   (mustache-context :data
    `((:static . "static")
      (:lambda . ,(lambda (text) nil)))
    :partials 'nil))
  "<>"
  (format nil "~A :: ~A" "Inverted Section"
          "Lambdas used for inverted sections should be considered truthy."))
 (is
  (mustache-render-to-string
   "{{#lambda}}FILE{{/lambda}} != {{#lambda}}LINE{{/lambda}}"
   (mustache-context :data
    `((:lambda . ,(lambda (text) (format nil "__~a__" text))))
    :partials 'nil))
  "__FILE__ != __LINE__"
  (format nil "~A :: ~A" "Section - Multiple Calls"
          "Lambdas used for sections should not be cached."))
 (is
  (mustache-render-to-string "{{= | | =}}<|#lambda|-|/lambda|>"
   (mustache-context :data
    `((:planet . "Earth")
      (:lambda . ,(lambda (text) (format nil "~a{{planet}} => |planet|~a" text text))))
    :partials 'nil))
  "<-{{planet}} => Earth->"
  (format nil "~A :: ~A" "Section - Alternate Delimiters"
          "Lambdas used for sections should parse with the current delimiters."))
 (is
  (mustache-render-to-string "<{{#lambda}}-{{/lambda}}>"
   (mustache-context :data
    `((:planet . "Earth")
      (:lambda . ,(lambda (text) (format nil "~a{{planet}}~a" text text))))
    :partials 'nil))
  "<-Earth->"
  (format nil "~A :: ~A" "Section - Expansion"
          "Lambdas used for sections should have their results parsed."))
 (is
  (mustache-render-to-string "<{{#lambda}}{{x}}{{/lambda}}>"
   (mustache-context :data
    `((:x . "Error!")
      (:lambda . ,(lambda (text) (if (equal text "{{x}}") "yes" "no"))))
    :partials 'nil))
  "<yes>"
  (format nil "~A :: ~A" "Section"
          "Lambdas used for sections should receive the raw section string."))
 (is
  (mustache-render-to-string "<{{lambda}}{{{lambda}}}"
   (mustache-context :data
    `((:lambda . ,(lambda () ">")))
    :partials 'nil))
  "<&gt;>"
  (format nil "~A :: ~A" "Escaping"
          "Lambda results should be appropriately escaped."))
 (is
  (mustache-render-to-string "{{lambda}} == {{{lambda}}} == {{lambda}}"
   (mustache-context :data
    `((:lambda . ,(let ((calls 0)) (lambda () (incf calls)))))
    :partials 'nil))
  "1 == 2 == 3"
  (format nil "~A :: ~A" "Interpolation - Multiple Calls"
          "Interpolated lambdas should not be cached."))
 (is
  (mustache-render-to-string "{{= | | =}}
Hello, (|&lambda|)!"
   (mustache-context :data
    `((:planet . "world")
      (:lambda . ,(lambda () "|planet| => {{planet}}")))
    :partials 'nil))
  "Hello, (|planet| => world)!"
  (format nil "~A :: ~A" "Interpolation - Alternate Delimiters"
          "A lambda's return value should parse with the default delimiters."))
 (is
  (mustache-render-to-string "Hello, {{lambda}}!"
   (mustache-context :data
    `((:planet . "world")
      (:lambda . ,(lambda () "{{planet}}")))
    :partials 'nil))
  "Hello, world!"
  (format nil "~A :: ~A" "Interpolation - Expansion"
          "A lambda's return value should be parsed."))
 (is
  (mustache-render-to-string "Hello, {{lambda}}!"
   (mustache-context :data
    `((:lambda . ,(lambda () "world")))
    :partials 'nil))
  "Hello, world!"
  (format nil "~A :: ~A" "Interpolation"
          "A lambda's return value should be interpolated."))
)
