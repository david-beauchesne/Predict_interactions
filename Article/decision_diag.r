library(DiagrammeR)
grViz("

digraph boxes_and_circles{

    node [shape = box
            # fixedsize = TRUE
            # width = 2.5
            ]
            1 [label = <<B>S1) </B>I(<I>T<sub><font point-size='8'>C</font></sub>,T<sub><font point-size='8'>R</font></sub></I>) in <I>S0</I>?>]
            2 [label = <<B>S2) </B><I>T<sub><font point-size='8'>R </font></sub></I> in <I>S1</I>?>]
            3 [label = <<B>S3) </B>Add <I>T<sub><font point-size='8'>R </font></sub></I> to<br/>predictions>]
            4 [label = <<B>S4) </B><I>K </I> most similar<br/>resource <I>T<sub><font point-size='8'>R&apos;</font></sub></I> in <I>S1</I>>]
            5 [label = <<B>S5) </B><I>T<sub><font point-size='8'>R&apos; </font></sub></I> in <I>C<SUB><font point-size='8'>R</font></SUB></I>?>]
            6 [label = <<B>S6) </B>Add weight = <I>t(T<sub><font point-size='8'>R</font></sub>,T<sub><font point-size='8'>R&apos;</font></sub>,w<sub><font point-size='8'>t</font></sub>) </I><br/>to <I>T<sub><font point-size='8'>R&apos; </font></sub></I>in <I>C<SUB><font point-size='8'>R </font></SUB></I> if<br/><I>t </I> &gt; minimum threshold>]
            7 [label = <<B>S7) </B>Add <I>T<sub><font point-size='8'>R&apos; </font></sub></I>to <I>C<SUB><font point-size='8'>R </font></SUB></I> with<br/>weight = <I>t(T<sub><font point-size='8'>R</font></sub>,T<sub><font point-size='8'>R&apos;</font></sub>,w<sub><font point-size='8'>t</font></sub>) </I> if<br/><I>t </I> &gt; minimum threshold>]
            8 [label = <<B>S8) </B><I>K </I> most similar<br/>consumer <I>T<sub><font point-size='8'>C&apos;</font></sub></I>>]
            9 [label = <<B>S9) </B><I>T<sub><font point-size='8'>R </font></sub></I>in <I>S1</I>?>]
            10 [label = <<B>S10) </B><I>T<sub><font point-size='8'>R </font></sub></I>in <I>C<SUB><font point-size='8'>R</font></SUB></I>?>]
            11 [label = <<B>S11) </B>Add 1 to <I>T<sub><font point-size='8'>R </font></sub></I><br/>weight in <I>C<SUB><font point-size='8'>R</font></SUB></I>>]
            12 [label = <<B>S12) </B>Add <I>T<sub><font point-size='8'>R </font></sub></I>to <I>C<SUB><font point-size='8'>R </font></SUB></I><br/>with weight = 1>]
            13 [label = <<B>S13) </B><I>K </I> most similar<br/>resource <I>T<sub><font point-size='8'>R&apos; </font></sub></I>in <I>S1</I>>]
            14 [label = <<B>S14) </B><I>T<sub><font point-size='8'>R&apos; </font></sub></I>in <I>C<SUB><font point-size='8'>R</font></SUB></I>?>]
            15 [label = <<B>S15) </B>Add weight = <I>t(T<sub><font point-size='8'>R</font></sub>,T<sub><font point-size='8'>R&apos;</font></sub>,w<sub><font point-size='8'>t</font></sub>) </I><br/>to <I>T<sub><font point-size='8'>R&apos; </font></sub></I>in <I>C<SUB><font point-size='8'>R </font></SUB></I>if<br/><I>t </I> &gt; minimum threshold>]
            16 [label = <<B>S16) </B>Add <I>T<sub><font point-size='8'>R&apos; </font></sub></I>to <I>C<SUB><font point-size='8'>R </font></SUB></I>with<br/> weight = <I>t(T<sub><font point-size='8'>R</font></sub>,T<sub><font point-size='8'>R&apos;</font></sub>,w<sub><font point-size='8'>t</font></sub>) </I> if<br/><I>t </I> &gt; minimum threshold>]
            17 [label = <<B>S17) </B>Add <I>T<sub><font point-size='8'>R </font></sub></I>or <I>T<sub><font point-size='8'>R&apos; </font></sub></I>to predictions if weight &gt; minimum weight>]

1 -> 2 [label = 'Yes', headport = 'n', tailport = 'w']
1 -> 3 [color = 'transparent']
1 -> 4 [color = 'transparent']
1 -> 5 [color = 'transparent']
1 -> 6 [color = 'transparent']
1 -> 7 [color = 'transparent']
1 -> 8 [color = 'transparent']
2 -> 3 [label = 'Yes']
2 -> 4 [label = 'No']
4 -> 5
5 -> 6 [label = 'Yes']
5 -> 7 [label = 'No']
6 -> 17
7 -> 17
1 -> 8 [tailport = 'e']
8 -> 9
9 -> 10 [label = 'Yes']
10 -> 11 [label = 'Yes']
10 -> 12 [label = 'No']
11 -> 17
12 -> 17
9 -> 13 [label = 'No']
13 -> 14
14 -> 15 [label = 'Yes']
14 -> 16 [label = 'No']
15 -> 17
16 -> 17

graph [ranksep = 0.15
        rank = sink
        # rankdir = LR
        # splines = ortho
        ]


}
")
