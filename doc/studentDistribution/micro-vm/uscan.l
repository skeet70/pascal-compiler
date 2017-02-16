%{
/**************************************************************************
 * uCode Scanner Specification
 *   The scanner reads individual tokens that are then "parsed" by the
 *   uMachine loader.
 *************************************************************************/
#include "uscan.h"

uTOKEN yytok;
%}
UNSIGNED        [0-9]+
INTEGER         [+-]?{UNSIGNED}

WHITESPACE      [\t ]+
COMMENT         ";".*
EOLN            "\n"

LABEL           [Ll]{UNSIGNED}
IMMEDIATE       "#"{INTEGER}

REGISTER        [Dd]{UNSIGNED}
INDEXED         {INTEGER}"("{REGISTER}")"
INDIRECT        "@"{INDEXED}

STACK_REGISTER  [Ss][Pp]
STACK_INDEXED   {INTEGER}"("{STACK_REGISTER}")"
STACK_INDIRECT  "@"{STACK_INDEXED}

%%
[\t ]+      						{ yytok =  tWhitespace;     return 1; }
";".*         						{ yytok =  tComment;        return 1; }
[\n\r]+            					{ yytok =  tEOLN;           return 1; }
                                             
"#"[+-]?[0-9]+       				{ yytok =  tImmediate;      return 1; }
"#"[+-]?[0-9]+"."[0-9]+				{ yytok =  tImmediateF;		return 1; }
"#"[+-]?[0-9]+"."[0-9]+[eE][+-]?[0-9]+ { yytok = tImmediateF;   return 1; }
"#\"".*\"       { yytok = tImmediateS; return 1; }

[Dd][0-9]+        					{ yytok =  tRegister;       return 1; }
[+-]?[0-9]+"("[Dd][0-9]+")"         { yytok =  tIndexed;        return 1; }
"@"[+-]?[0-9]+"("[Dd][0-9]+")"	    { yytok =  tIndirect;       return 1; }
                                             
[Ss][Pp]							{ yytok =  tStackRegister;  return 1; }
[+-]?[0-9]+"("[Ss][Pp]")" 		    { yytok =  tStackIndexed;   return 1; }
"@"[+-]?[0-9]+"("[Ss][Pp]")"	    { yytok =  tStackIndirect;  return 1; }
                                             
[Ll][0-9]+           		{ yytok =  tLabel;          return 1; }
                                             
[Hh][Ll][Tt]      			{ yytok =  tHLT;            return 1; }
                                             
[Rr][Dd]          			{ yytok =  tRD;             return 1; }
[Rr][Dd][Ff]	  			{ yytok =  tRDF;			return 1; }
[Rr][Dd][Ss]	  			{ yytok =  tRDS;			return 1; }
[Ww][Rr][Tt]      			{ yytok =  tWRT;            return 1; }
[Ww][Rr][Tt][Ss]  			{ yytok =  tWRTS;           return 1; }
[Ww][Rr][Tt][Ll][Nn]      	{ yytok =  tWRTLN;          return 1; }
[Ww][Rr][Tt][Ll][Nn][Ss]  	{ yytok =  tWRTLNS;         return 1; }
                                             
[Mm][Oo][Vv]      { yytok =  tMOV;            return 1; }
[Nn][Ee][Gg]      { yytok =  tNEG;            return 1; }
[Aa][Dd][Dd]      { yytok =  tADD;            return 1; }
[Ss][Uu][Bb]      { yytok =  tSUB;            return 1; }
[Mm][Uu][Ll]      { yytok =  tMUL;            return 1; }
[Dd][Ii][Vv]      { yytok =  tDIV;            return 1; }
[Mm][Oo][Dd]	  { yytok =  tMOD;			  return 1; }

[Cc][Aa][Ss][Tt][Ii] { yytok = tCASTI;        return 1; }
[Cc][Aa][Ss][Tt][Ff] { yytok = tCASTF;        return 1; }

[Nn][Ee][Gg][Ff]  { yytok =  tNEGF;           return 1; }
[Aa][Dd][Dd][Ff]  { yytok =  tADDF;           return 1; }
[Ss][Uu][Bb][Ff]  { yytok =  tSUBF;           return 1; }
[Mm][Uu][Ll][Ff]  { yytok =  tMULF;           return 1; }
[Dd][Ii][Vv][Ff]  { yytok =  tDIVF;           return 1; }
                                             
[Pp][Uu][Ss][Hh]  { yytok =  tPUSH;           return 1; }
[Pp][Oo][Pp]      { yytok =  tPOP ;           return 1; }
[Nn][Ee][Gg][Ss]  { yytok =  tNEGS;           return 1; }
[Aa][Dd][Dd][Ss]  { yytok =  tADDS;           return 1; }
[Ss][Uu][Bb][Ss]  { yytok =  tSUBS;           return 1; }
[Mm][Uu][Ll][Ss]  { yytok =  tMULS;           return 1; }
[Dd][Ii][Vv][Ss]  { yytok =  tDIVS;           return 1; }
[Mm][Oo][Dd][Ss]  { yytok =  tMODS;			  return 1; }

[Nn][Ee][Gg][Ss][Ff]  { yytok =  tNEGSF;           return 1; }
[Aa][Dd][Dd][Ss][Ff]  { yytok =  tADDSF;           return 1; }
[Ss][Uu][Bb][Ss][Ff]  { yytok =  tSUBSF;           return 1; }
[Mm][Uu][Ll][Ss][Ff]  { yytok =  tMULSF;           return 1; }
[Dd][Ii][Vv][Ss][Ff]  { yytok =  tDIVSF;           return 1; }
             
[Cc][Aa][Ss][Tt][Ss][Ii] { yytok = tCASTSI;        return 1; }
[Cc][Aa][Ss][Tt][Ss][Ff] { yytok = tCASTSF;        return 1; }
      
[Ll][0-9]+[:]        { yytok =  tLAB ;           return 1; }

[Aa][Nn][Dd][Ss]							{ yytok = tANDS;		return 1; }
[Oo][Rr][Ss]								{ yytok = tORS;			return 1; }
[Nn][Oo][Tt][Ss]							{ yytok = tNOTS;		return 1; }

[Cc][Mm][Pp][Ee][Qq][Ss]					{ yytok = tCMPEQS;		return 1; }
[Cc][Mm][Pp][Gg][Ee][Ss]					{ yytok = tCMPGES;		return 1; }
[Cc][Mm][Pp][Gg][Tt][Ss]					{ yytok = tCMPGTS;		return 1; }
[Cc][Mm][Pp][Ll][Ee][Ss]					{ yytok = tCMPLES;		return 1; }
[Cc][Mm][Pp][Ll][Tt][Ss]					{ yytok = tCMPLTS;		return 1; }
[Cc][Mm][Pp][Nn][Ee][Ss]					{ yytok = tCMPNES;		return 1; }

[Cc][Mm][Pp][Ee][Qq][Ss][Ff]				{ yytok = tCMPEQSF;		return 1; }
[Cc][Mm][Pp][Gg][Ee][Ss][Ff]				{ yytok = tCMPGESF;		return 1; }
[Cc][Mm][Pp][Gg][Tt][Ss][Ff]				{ yytok = tCMPGTSF;		return 1; }
[Cc][Mm][Pp][Ll][Ee][Ss][Ff]				{ yytok = tCMPLESF;		return 1; }
[Cc][Mm][Pp][Ll][Tt][Ss][Ff]				{ yytok = tCMPLTSF;		return 1; }
[Cc][Mm][Pp][Nn][Ee][Ss][Ff]				{ yytok = tCMPNESF;		return 1; }

[Bb][Rr][Tt][Ss]      { yytok = tBRTS;			  return 1; }
[Bb][Rr][Ff][Ss]      { yytok = tBRFS;			  return 1; }
                                             
[Bb][Rr]          { yytok =  tBR  ;           return 1; }
[Bb][Ee][Qq]      { yytok =  tBEQ ;           return 1; }
[Bb][Gg][Ee]      { yytok =  tBGE ;           return 1; }
[Bb][Gg][Tt]      { yytok =  tBGT ;           return 1; }
[Bb][Ll][Ee]      { yytok =  tBLE ;           return 1; }
[Bb][Ll][Tt]      { yytok =  tBLT ;           return 1; }
[Bb][Nn][Ee]      { yytok =  tBNE ;           return 1; }

[Bb][Ee][Qq][Ff]  { yytok =  tBEQF ;          return 1; }
[Bb][Gg][Ee][Ff]  { yytok =  tBGEF ;          return 1; }
[Bb][Gg][Tt][Ff]  { yytok =  tBGTF ;          return 1; }
[Bb][Ll][Ee][Ff]  { yytok =  tBLEF ;          return 1; }
[Bb][Ll][Tt][Ff]  { yytok =  tBLTF ;          return 1; }
[Bb][Nn][Ee][Ff]  { yytok =  tBNEF ;          return 1; }
                                             
[Cc][Aa][Ll][Ll]  { yytok =  tCALL;           return 1; }
[Rr][Ee][Tt]      { yytok =  tRET ;           return 1; }

[Pp][Rr][Tt][Ss]  { yytok =  tPRTS;           return 1; }
[Pp][Rr][Tt][Rr]  { yytok =  tPRTR;           return 1; }
<<EOF>>           { yytok =  tEOF;            return 1; }


.                 {
                    /* Unknown character found (error) */
					int inChar = yyinput();
					printf("uscan.l: Unrecognzied character:%c.\n", inChar);

                    while (  ((char) inChar != '\0')
                          && ((char) inChar != '\n')
                          && ((char) inChar != '\t')
                          && ((char) inChar != ' ' ))
                    {
                      yytext[yyleng++] = inChar;
                      inChar = yyinput();
					  printf("uscan.l: Unrecognzied character:%c.\n", inChar);
                    }

                    yytext[yyleng] = '\0';
                    unput(inChar);

                    yytok = tError;
                    return 1;
                  }
%%

