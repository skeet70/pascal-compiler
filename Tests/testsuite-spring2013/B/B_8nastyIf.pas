;Parse complete, stop poking me!
;Autogenerated Parse
;DO NOT EDIT
MOV SP D0
ADD SP #1 SP
ADD SP #1 SP
ADD SP #1 SP
ADD SP #1 SP
ADD SP #1 SP
ADD SP #1 SP
PUSH #55
CASTSI
CASTSI
POP 1(D0)
PUSH #65
CASTSI
CASTSI
POP 2(D0)
PUSH #10
CASTSI
CASTSI
POP 3(D0)
PUSH 1(D0)
CASTSI
PUSH 2(D0)
CASTSI
CMPLTS
BRFS L1
PUSH #9000000
CASTSI
WRTLNS
BR L2
L1:
RD 1(D0)
L2:
PUSH 1(D0)
CASTSI
PUSH 2(D0)
CASTSI
CMPLTS
BRFS L3
PUSH #1
CASTSI
WRTLNS
BR L4
L3:
PUSH #20
CASTSI
CASTSI
POP 1(D0)
L4:
PUSH 1(D0)
CASTSI
PUSH 3(D0)
CASTSI
CMPEQS
BRFS L5
PUSH 2(D0)
CASTSI
PUSH 3(D0)
CASTSI
CMPGTS
BRFS L7
PUSH 1(D0)
CASTSI
PUSH 3(D0)
CASTSI
CMPGTS
BRFS L9
PUSH #100000
CASTSI
WRTLNS
BR L10
L9:
PUSH #5
CASTSI
PUSH #2
CASTSI
ADDS
PUSH #1
CASTSI
PUSH #6
CASTSI
ADDS
CMPEQS
BRFS L11
PUSH #2
CASTSI
WRTLNS
BR L12
L11:
L12:
L10:
BR L8
L7:
L8:
BR L6
L5:
PUSH #100000
CASTSI
WRTLNS
L6:
PUSH 1(D0)
CASTSI
PUSH 2(D0)
CASTSI
CMPGTS
PUSH 1(D0)
CASTSI
PUSH 3(D0)
CASTSI
CMPGTS
ORS
PUSH 1(D0)
CASTSI
PUSH 3(D0)
CASTSI
CMPEQS
ORS
BRFS L13
PUSH 1(D0)
CASTSI
PUSH 3(D0)
CASTSI
CMPNES
BRFS L15
PUSH 2(D0)
CASTSI
PUSH 1(D0)
CASTSI
CMPLTS
PUSH 3(D0)
CASTSI
PUSH 3(D0)
CASTSI
CMPNES
ORS
BRFS L17
PUSH 2(D0)
CASTSI
PUSH #55
CASTSI
CMPEQS
BRFS L19
PUSH #100000
CASTSI
WRTLNS
BR L20
L19:
PUSH #3
CASTSI
WRTLNS
L20:
BR L18
L17:
PUSH #200000
CASTSI
WRTLNS
L18:
BR L16
L15:
L16:
BR L14
L13:
PUSH #300000
CASTSI
WRTLNS
L14:
MOV D0 SP
HLT
