" Vim syntax file
" Language:     gherkin
" License:      Same as VIM
" Authors:      Chris McCormick <chris@mccormick.cx>
"               Morten Linderud <mcfoxax@gmail.com>
"               Alejandro Gómez <alejandro@dialelo.com>
" URL:          http://github.com/chr15m/vim-gherkin
"
" Modified version of the clojure syntax file: https://github.com/guns/vim-clojure-static/blob/master/syntax/clojure.vim
if exists("b:current_syntax")
    finish
endif


syntax keyword gherkinConstant null nil
syntax keyword gherkinBoolean f t
syntax keyword gherkinSpecial & nil t quote fn if set! def do recur eq? nil? car cdr cons list eval apply read + - / mod < > cons? symbol? number? string? fn? gensym random exit println sh sh! load-file gc error type str
"syntax keyword gherkinSpecial macro-error defmacro-alias let if-python2 def setv fn lambda
"syntax keyword gherkinException throw raise try except catch
"syntax keyword gherkinCond cond if-not lisp-if lif when unless
"syntax keyword gherkinRepeat loop for* while
"syntax keyword gherkinDefine defmacro/g! defmain defn-alias defun-alias defmulti defnc defclass defmacro defreader defn defun
"syntax keyword gherkinMacro for with car cdr -> ->> with-gensyms ap-if ap-each ap-each-while ap-map ap-map-when ap-filter ap-reject ap-dotimes ap-first ap-last ap-reduce defnc fnc fnr route-with-methods route post-route put-route delete-route profile/calls profile/cpu walk postwalk prewalk macroexpand-all
"syntax keyword gherkinFunc curry --trampoline-- recursive-replace _numeric-check coll? cons cons? keyword? dec disassemble distinct drop empty? even? every? fake-source-positions flatten _flatten float? gensym calling-module-name first identity inc instance? integer integer? integer-char? iterable? iterate iterator? list* macroexpand macroexpand-1 neg? none? nil? numeric? nth odd? pos? remove rest repeatedly second some string string? take take-nth zero? list quote quasiquote unquote unquote-splicing eval do progn if break print continue assert global yield yield-from from import get . del slice assoc with-decorator with* , list-comp set-comp dict-comp genexpr apply not ~ require and or = != < <= > >= is in is-not not-in % / // ** << >> ^ & + * - += /= //= *= -= %= **= <<= >>= ^= &= HyExpression HyList dispatch-reader-macro eval-and-compile eval-when-compile HyCons HyInteger HyFloat HyComplex HySymbol HyString HyKeyword
" Not used at this moment
"syntax keyword gherkinVariable


" Keywords are symbols:
"   static Pattern symbolPat = Pattern.compile("[:]?([\\D&&[^/]].*/)?([\\D&&[^/]][^/]*)");
" But they:
"   * Must not end in a : or /
"   * Must not have two adjacent colons except at the beginning
"   * Must not contain any reader metacharacters except for ' and #
syntax match gherkinKeyword "\v<:{1,2}%([^ \n\r\t()\[\]{}";@^`~\\%/]+/)*[^ \n\r\t()\[\]{}";@^`~\\%/]+:@<!>"

syntax match gherkinStringEscape "\v\\%([\\btnfr"]|u\x{4}|[0-3]\o{2}|\o{1,2})" contained

syntax region gherkinString start=/"/ skip=/\\\\\|\\"/ end=/"/ contains=gherkinStringEscape

syntax match gherkinCharacter "\\."
syntax match gherkinCharacter "\\o\%([0-3]\o\{2\}\|\o\{1,2\}\)"
syntax match gherkinCharacter "\\u\x\{4\}"
syntax match gherkinCharacter "\\space"
syntax match gherkinCharacter "\\tab"
syntax match gherkinCharacter "\\newline"
syntax match gherkinCharacter "\\return"
syntax match gherkinCharacter "\\backspace"
syntax match gherkinCharacter "\\formfeed"

syntax match gherkinSymbol "\v%([a-zA-Z!$&*_+=|<.>?-]|[^\x00-\x7F])+%(:?%([a-zA-Z0-9!#$%&*_+=|'<.>/?-]|[^\x00-\x7F]))*[#:]@<!"

let s:radix_chars = "0123456789abcdefghijklmnopqrstuvwxyz"
for s:radix in range(2, 36)
    execute 'syntax match gherkinNumber "\v\c<[-+]?' . s:radix . 'r[' . strpart(s:radix_chars, 0, s:radix) . ']+>"'
endfor
unlet! s:radix_chars s:radix

syntax match gherkinNumber "\v<[-+]?%(0\o*|0x\x+|[1-9]\d*)N?>"
syntax match gherkinNumber "\v<[-+]?%(0|[1-9]\d*|%(0|[1-9]\d*)\.\d*)%(M|[eE][-+]?\d+)?>"
syntax match gherkinNumber "\v<[-+]?%(0|[1-9]\d*)/%(0|[1-9]\d*)>"

syntax match gherkinVarArg "&"

syntax match gherkinQuote "'"
syntax match gherkinQuote "`"
syntax match gherkinUnquote "\~"
syntax match gherkinUnquote "\~@"
syntax match gherkinMeta "\^"
syntax match gherkinDeref "@"
syntax match gherkinDispatch "\v#[\^'=<_]?"

" gherkin permits no more than 20 params.
syntax match gherkinAnonArg "%\(20\|1\d\|[1-9]\|&\)\?"

syntax match   gherkinRegexpEscape  "\v\\%(\\|[tnrfae]|c\u|0[0-3]?\o{1,2}|x%(\x{2}|\{\x{1,6}\})|u\x{4})" contained display
syntax region  gherkinRegexpQuoted  start=/\v\<@!\\Q/ms=e+1 skip=/\v\\\\|\\"/ end=/\\E/me=s-1 end=/"/me=s-1 contained
syntax region  gherkinRegexpQuote   start=/\v\<@!\\Q/       skip=/\v\\\\|\\"/ end=/\\E/       end=/"/me=s-1 contains=gherkinRegexpQuoted keepend contained
syntax cluster gherkinRegexpEscapes contains=gherkinRegexpEscape,gherkinRegexpQuote

syntax match gherkinRegexpPosixCharClass "\v\\[pP]\{%(ASCII|Alnum|Alpha|Blank|Cntrl|Digit|Graph|Lower|Print|Punct|Space|Upper|XDigit)\}" contained display
syntax match gherkinRegexpJavaCharClass "\v\\[pP]\{java%(Alphabetic|Defined|Digit|ISOControl|IdentifierIgnorable|Ideographic|JavaIdentifierPart|JavaIdentifierStart|Letter|LetterOrDigit|LowerCase|Mirrored|SpaceChar|TitleCase|UnicodeIdentifierPart|UnicodeIdentifierStart|UpperCase|Whitespace)\}" contained display
syntax match gherkinRegexpUnicodeCharClass "\v\\[pP]\{\cIs%(alnum|alphabetic|assigned|blank|control|digit|graph|hex_digit|hexdigit|ideographic|letter|lowercase|noncharacter_code_point|noncharactercodepoint|print|punctuation|titlecase|uppercase|white_space|whitespace|word)\}" contained display
syntax match gherkinRegexpUnicodeCharClass "\v\\[pP]%(C|L|M|N|P|S|Z)" contained display
syntax match gherkinRegexpUnicodeCharClass "\v\\[pP]\{%(Is|gc\=|general_category\=)?%(C[cfnos]|L[CDlmotu]|M[cen]|N[dlo]|P[cdefios]|S[ckmo]|Z[lps])\}" contained display
syntax match gherkinRegexpUnicodeCharClass "\v\\[pP]\{\c%(Is|sc\=|script\=)%(arab|arabic|armenian|armi|armn|avestan|avst|bali|balinese|bamu|bamum|batak|batk|beng|bengali|bopo|bopomofo|brah|brahmi|brai|braille|bugi|buginese|buhd|buhid|canadian_aboriginal|cans|cari|carian|cham|cher|cherokee|common|copt|coptic|cprt|cuneiform|cypriot|cyrillic|cyrl|deseret|deva|devanagari|dsrt|egyp|egyptian_hieroglyphs|ethi|ethiopic|geor|georgian|glag|glagolitic|goth|gothic|greek|grek|gujarati|gujr|gurmukhi|guru|han|hang|hangul|hani|hano|hanunoo|hebr|hebrew|hira|hiragana|imperial_aramaic|inherited|inscriptional_pahlavi|inscriptional_parthian|ital|java|javanese|kaithi|kali|kana|kannada|katakana|kayah_li|khar|kharoshthi|khmer|khmr|knda|kthi|lana|lao|laoo|latin|latn|lepc|lepcha|limb|limbu|linb|linear_b|lisu|lyci|lycian|lydi|lydian|malayalam|mand|mandaic|meetei_mayek|mlym|mong|mongolian|mtei|myanmar|mymr|new_tai_lue|nko|nkoo|ogam|ogham|ol_chiki|olck|old_italic|old_persian|old_south_arabian|old_turkic|oriya|orkh|orya|osma|osmanya|phag|phags_pa|phli|phnx|phoenician|prti|rejang|rjng|runic|runr|samaritan|samr|sarb|saur|saurashtra|shavian|shaw|sinh|sinhala|sund|sundanese|sylo|syloti_nagri|syrc|syriac|tagalog|tagb|tagbanwa|tai_le|tai_tham|tai_viet|tale|talu|tamil|taml|tavt|telu|telugu|tfng|tglg|thaa|thaana|thai|tibetan|tibt|tifinagh|ugar|ugaritic|unknown|vai|vaii|xpeo|xsux|yi|yiii|zinh|zyyy|zzzz)\}" contained display
syntax match gherkinRegexpUnicodeCharClass "\v\\[pP]\{\c%(In|blk\=|block\=)%(aegean numbers|aegean_numbers|aegeannumbers|alchemical symbols|alchemical_symbols|alchemicalsymbols|alphabetic presentation forms|alphabetic_presentation_forms|alphabeticpresentationforms|ancient greek musical notation|ancient greek numbers|ancient symbols|ancient_greek_musical_notation|ancient_greek_numbers|ancient_symbols|ancientgreekmusicalnotation|ancientgreeknumbers|ancientsymbols|arabic|arabic presentation forms-a|arabic presentation forms-b|arabic supplement|arabic_presentation_forms_a|arabic_presentation_forms_b|arabic_supplement|arabicpresentationforms-a|arabicpresentationforms-b|arabicsupplement|armenian|arrows|avestan|balinese|bamum|bamum supplement|bamum_supplement|bamumsupplement|basic latin|basic_latin|basiclatin|batak|bengali|block elements|block_elements|blockelements|bopomofo|bopomofo extended|bopomofo_extended|bopomofoextended|box drawing|box_drawing|boxdrawing|brahmi|braille patterns|braille_patterns|braillepatterns|buginese|buhid|byzantine musical symbols|byzantine_musical_symbols|byzantinemusicalsymbols|carian|cham|cherokee|cjk compatibility|cjk compatibility forms|cjk compatibility ideographs|cjk compatibility ideographs supplement|cjk radicals supplement|cjk strokes|cjk symbols and punctuation|cjk unified ideographs|cjk unified ideographs extension a|cjk unified ideographs extension b|cjk unified ideographs extension c|cjk unified ideographs extension d|cjk_compatibility|cjk_compatibility_forms|cjk_compatibility_ideographs|cjk_compatibility_ideographs_supplement|cjk_radicals_supplement|cjk_strokes|cjk_symbols_and_punctuation|cjk_unified_ideographs|cjk_unified_ideographs_extension_a|cjk_unified_ideographs_extension_b|cjk_unified_ideographs_extension_c|cjk_unified_ideographs_extension_d|cjkcompatibility|cjkcompatibilityforms|cjkcompatibilityideographs|cjkcompatibilityideographssupplement|cjkradicalssupplement|cjkstrokes|cjksymbolsandpunctuation|cjkunifiedideographs|cjkunifiedideographsextensiona|cjkunifiedideographsextensionb|cjkunifiedideographsextensionc|cjkunifiedideographsextensiond|combining diacritical marks|combining diacritical marks for symbols|combining diacritical marks supplement|combining half marks|combining marks for symbols|combining_diacritical_marks|combining_diacritical_marks_supplement|combining_half_marks|combining_marks_for_symbols|combiningdiacriticalmarks|combiningdiacriticalmarksforsymbols|combiningdiacriticalmarkssupplement|combininghalfmarks|combiningmarksforsymbols|common indic number forms|common_indic_number_forms|commonindicnumberforms|control pictures|control_pictures|controlpictures|coptic|counting rod numerals|counting_rod_numerals|countingrodnumerals|cuneiform|cuneiform numbers and punctuation|cuneiform_numbers_and_punctuation|cuneiformnumbersandpunctuation|currency symbols|currency_symbols|currencysymbols|cypriot syllabary|cypriot_syllabary|cypriotsyllabary|cyrillic|cyrillic extended-a|cyrillic extended-b|cyrillic supplement|cyrillic supplementary|cyrillic_extended_a|cyrillic_extended_b|cyrillic_supplementary|cyrillicextended-a|cyrillicextended-b|cyrillicsupplement|cyrillicsupplementary|deseret|devanagari|devanagari extended|devanagari_extended|devanagariextended|dingbats|domino tiles|domino_tiles|dominotiles|egyptian hieroglyphs|egyptian_hieroglyphs|egyptianhieroglyphs|emoticons|enclosed alphanumeric supplement|enclosed alphanumerics|enclosed cjk letters and months|enclosed ideographic supplement|enclosed_alphanumeric_supplement|enclosed_alphanumerics|enclosed_cjk_letters_and_months|enclosed_ideographic_supplement|enclosedalphanumerics|enclosedalphanumericsupplement|enclosedcjklettersandmonths|enclosedideographicsupplement|ethiopic|ethiopic extended|ethiopic extended-a|ethiopic supplement|ethiopic_extended|ethiopic_extended_a|ethiopic_supplement|ethiopicextended|ethiopicextended-a|ethiopicsupplement|general punctuation|general_punctuation|generalpunctuation|geometric shapes|geometric_shapes|geometricshapes|georgian|georgian supplement|georgian_supplement|georgiansupplement|glagolitic|gothic|greek|greek and coptic|greek extended|greek_extended|greekandcoptic|greekextended|gujarati|gurmukhi|halfwidth and fullwidth forms|halfwidth_and_fullwidth_forms|halfwidthandfullwidthforms|hangul compatibility jamo|hangul jamo|hangul jamo extended-a|hangul jamo extended-b|hangul syllables|hangul_compatibility_jamo|hangul_jamo|hangul_jamo_extended_a|hangul_jamo_extended_b|hangul_syllables|hangulcompatibilityjamo|hanguljamo|hanguljamoextended-a|hanguljamoextended-b|hangulsyllables|hanunoo|hebrew|high private use surrogates|high surrogates|high_private_use_surrogates|high_surrogates|highprivateusesurrogates|highsurrogates|hiragana|ideographic description characters|ideographic_description_characters|ideographicdescriptioncharacters|imperial aramaic|imperial_aramaic|imperialaramaic|inscriptional pahlavi|inscriptional parthian|inscriptional_pahlavi|inscriptional_parthian|inscriptionalpahlavi|inscriptionalparthian|ipa extensions|ipa_extensions|ipaextensions|javanese|kaithi|kana supplement|kana_supplement|kanasupplement|kanbun|kangxi radicals|kangxi_radicals|kangxiradicals|kannada|katakana|katakana phonetic extensions|katakana_phonetic_extensions|katakanaphoneticextensions|kayah li|kayah_li|kayahli|kharoshthi|khmer|khmer symbols|khmer_symbols|khmersymbols|lao|latin extended additional|latin extended-a|latin extended-b|latin extended-c|latin extended-d|latin-1 supplement|latin-1supplement|latin_1_supplement|latin_extended_a|latin_extended_additional|latin_extended_b|latin_extended_c|latin_extended_d|latinextended-a|latinextended-b|latinextended-c|latinextended-d|latinextendedadditional|lepcha|letterlike symbols|letterlike_symbols|letterlikesymbols|limbu|linear b ideograms|linear b syllabary|linear_b_ideograms|linear_b_syllabary|linearbideograms|linearbsyllabary|lisu|low surrogates|low_surrogates|lowsurrogates|lycian|lydian|mahjong tiles|mahjong_tiles|mahjongtiles|malayalam|mandaic|mathematical alphanumeric symbols|mathematical operators|mathematical_alphanumeric_symbols|mathematical_operators|mathematicalalphanumericsymbols|mathematicaloperators|meetei mayek|meetei_mayek|meeteimayek|miscellaneous mathematical symbols-a|miscellaneous mathematical symbols-b|miscellaneous symbols|miscellaneous symbols and arrows|miscellaneous symbols and pictographs|miscellaneous technical|miscellaneous_mathematical_symbols_a|miscellaneous_mathematical_symbols_b|miscellaneous_symbols|miscellaneous_symbols_and_arrows|miscellaneous_symbols_and_pictographs|miscellaneous_technical|miscellaneousmathematicalsymbols-a|miscellaneousmathematicalsymbols-b|miscellaneoussymbols|miscellaneoussymbolsandarrows|miscellaneoussymbolsandpictographs|miscellaneoustechnical|modifier tone letters|modifier_tone_letters|modifiertoneletters|mongolian|musical symbols|musical_symbols|musicalsymbols|myanmar|myanmar extended-a|myanmar_extended_a|myanmarextended-a|new tai lue|new_tai_lue|newtailue|nko|number forms|number_forms|numberforms|ogham|ol chiki|ol_chiki|olchiki|old italic|old persian|old south arabian|old turkic|old_italic|old_persian|old_south_arabian|old_turkic|olditalic|oldpersian|oldsoutharabian|oldturkic|optical character recognition|optical_character_recognition|opticalcharacterrecognition|oriya|osmanya|phags-pa|phags_pa|phaistos disc|phaistos_disc|phaistosdisc|phoenician|phonetic extensions|phonetic extensions supplement|phonetic_extensions|phonetic_extensions_supplement|phoneticextensions|phoneticextensionssupplement|playing cards|playing_cards|playingcards|private use area|private_use_area|privateusearea|rejang|rumi numeral symbols|rumi_numeral_symbols|ruminumeralsymbols|runic|samaritan|saurashtra|shavian|sinhala|small form variants|small_form_variants|smallformvariants|spacing modifier letters|spacing_modifier_letters|spacingmodifierletters|specials|sundanese|superscripts and subscripts|superscripts_and_subscripts|superscriptsandsubscripts|supplemental arrows-a|supplemental arrows-b|supplemental mathematical operators|supplemental punctuation|supplemental_arrows_a|supplemental_arrows_b|supplemental_mathematical_operators|supplemental_punctuation|supplementalarrows-a|supplementalarrows-b|supplementalmathematicaloperators|supplementalpunctuation|supplementary private use area-a|supplementary private use area-b|supplementary_private_use_area_a|supplementary_private_use_area_b|supplementaryprivateusearea-a|supplementaryprivateusearea-b|surrogates_area|syloti nagri|syloti_nagri|sylotinagri|syriac|tagalog|tagbanwa|tags|tai le|tai tham|tai viet|tai xuan jing symbols|tai_le|tai_tham|tai_viet|tai_xuan_jing_symbols|taile|taitham|taiviet|taixuanjingsymbols|tamil|telugu|thaana|thai|tibetan|tifinagh|transport and map symbols|transport_and_map_symbols|transportandmapsymbols|ugaritic|unified canadian aboriginal syllabics|unified canadian aboriginal syllabics extended|unified_canadian_aboriginal_syllabics|unified_canadian_aboriginal_syllabics_extended|unifiedcanadianaboriginalsyllabics|unifiedcanadianaboriginalsyllabicsextended|vai|variation selectors|variation selectors supplement|variation_selectors|variation_selectors_supplement|variationselectors|variationselectorssupplement|vedic extensions|vedic_extensions|vedicextensions|vertical forms|vertical_forms|verticalforms|yi radicals|yi syllables|yi_radicals|yi_syllables|yijing hexagram symbols|yijing_hexagram_symbols|yijinghexagramsymbols|yiradicals|yisyllables)\}" contained display

syntax match   gherkinRegexpPredefinedCharClass "\v%(\\[dDsSwW]|\.)" contained display
syntax cluster gherkinRegexpCharPropertyClasses contains=gherkinRegexpPosixCharClass,gherkinRegexpJavaCharClass,gherkinRegexpUnicodeCharClass
syntax cluster gherkinRegexpCharClasses         contains=gherkinRegexpPredefinedCharClass,gherkinRegexpCharClass,@gherkinRegexpCharPropertyClasses
syntax region  gherkinRegexpCharClass           start="\\\@<!\[" end="\\\@<!\]" contained contains=gherkinRegexpPredefinedCharClass,@gherkinRegexpCharPropertyClasses
syntax match   gherkinRegexpBoundary            "\\[bBAGZz]"   contained display
syntax match   gherkinRegexpBoundary            "[$^]"         contained display
syntax match   gherkinRegexpQuantifier          "[?*+][?+]\="  contained display
syntax match   gherkinRegexpQuantifier          "\v\{\d+%(,|,\d+)?}\??" contained display
syntax match   gherkinRegexpOr                  "|" contained display
syntax match   gherkinRegexpBackRef             "\v\\%([1-9]\d*|k\<[a-zA-z]+\>)" contained display

" Mode modifiers, mode-modified spans, lookaround, regular and atomic
" grouping, and named-capturing.
syntax match gherkinRegexpMod "\v\(@<=\?:"                        contained display
syntax match gherkinRegexpMod "\v\(@<=\?[xdsmiuU]*-?[xdsmiuU]+:?" contained display
syntax match gherkinRegexpMod "\v\(@<=\?%(\<?[=!]|\>)"            contained display
syntax match gherkinRegexpMod "\v\(@<=\?\<[a-zA-Z]+\>"            contained display

syntax region gherkinRegexpGroup start="\\\@<!(" matchgroup=gherkinRegexpGroup end="\\\@<!)" contained contains=gherkinRegexpMod,gherkinRegexpQuantifier,gherkinRegexpBoundary,gherkinRegexpEscape,@gherkinRegexpCharClasses
syntax region gherkinRegexp start=/\#"/ skip=/\\\\\|\\"/ end=/"/ contains=@gherkinRegexpCharClasses,gherkinRegexpEscape,gherkinRegexpQuote,gherkinRegexpBoundary,gherkinRegexpQuantifier,gherkinRegexpOr,gherkinRegexpBackRef,gherkinRegexpGroup keepend

syntax keyword gherkinCommentTodo contained FIXME XXX TODO FIXME: XXX: TODO:

syntax match gherkinComment ";.*$" contains=gherkinCommentTodo,@Spell
syntax match gherkinComment "\%^#!.*$"

syntax region gherkinSexp   matchgroup=gherkinParen start="("  matchgroup=gherkinParen end=")"  contains=TOP,@Spell
syntax region gherkinVector matchgroup=gherkinParen start="\[" matchgroup=gherkinParen end="\]" contains=TOP,@Spell
syntax region gherkinMap    matchgroup=gherkinParen start="{"  matchgroup=gherkinParen end="}"  contains=TOP,@Spell

" Highlight superfluous closing parens, brackets and braces.
syntax match gherkinError "]\|}\|)"

syntax sync fromstart

highlight default link gherkinConstant     Constant
highlight default link gherkinBoolean      Boolean
highlight default link gherkinCharacter    Character
highlight default link gherkinKeyword      Keyword
highlight default link gherkinNumber       Number
highlight default link gherkinString       String
highlight default link gherkinStringEscape Character

highlight default link gherkinRegexp                    Constant
highlight default link gherkinRegexpEscape              Character
highlight default link gherkinRegexpCharClass           SpecialChar
highlight default link gherkinRegexpPosixCharClass      gherkinRegexpCharClass
highlight default link gherkinRegexpJavaCharClass       gherkinRegexpCharClass
highlight default link gherkinRegexpUnicodeCharClass    gherkinRegexpCharClass
highlight default link gherkinRegexpPredefinedCharClass gherkinRegexpCharClass
highlight default link gherkinRegexpBoundary            SpecialChar
highlight default link gherkinRegexpQuantifier          SpecialChar
highlight default link gherkinRegexpMod                 SpecialChar
highlight default link gherkinRegexpOr                  SpecialChar
highlight default link gherkinRegexpBackRef             SpecialChar
highlight default link gherkinRegexpGroup               gherkinRegexp
highlight default link gherkinRegexpQuoted              gherkinString
highlight default link gherkinRegexpQuote               gherkinRegexpBoundary

highlight default link gherkinVariable  Identifier
highlight default link gherkinCond      Conditional
highlight default link gherkinDefine    Define
highlight default link gherkinException Exception
highlight default link gherkinFunc      Function
highlight default link gherkinMacro     Macro
highlight default link gherkinRepeat    Repeat

highlight default link gherkinSpecial   Special
highlight default link gherkinVarArg    Special
highlight default link gherkinQuote     SpecialChar
highlight default link gherkinUnquote   SpecialChar
highlight default link gherkinMeta      SpecialChar
highlight default link gherkinDeref     SpecialChar
highlight default link gherkinAnonArg   SpecialChar
highlight default link gherkinDispatch  SpecialChar

highlight default link gherkinComment     Comment
highlight default link gherkinCommentTodo Todo

highlight default link gherkinError     Error

highlight default link gherkinParen     Delimiter

let b:current_syntax = "gherkin"

" vim:sts=4:sw=4:ts=4:et:smc=20000
