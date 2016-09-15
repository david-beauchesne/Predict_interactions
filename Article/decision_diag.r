# library(DiagrammeR)
grViz("

digraph boxes_and_circles{

    node [shape = box
            # fixedsize = TRUE
            # width = 2.5
            ]
            1 [label = <I(<I>T<SUB><font point-size='8'>C</font></SUB></I>) in <I>S0</I>?>]
            2 [label = <I(<I>T<sub><font point-size='8'>C</font></sub>,T<sub><font point-size='8'>R</font></sub></I>) in <I>S1</I>?>]
            3 [label = <Add I(<I>T<sub><font point-size='8'>C</font></sub>,T<sub><font point-size='8'>R</font></sub></I>) to<br/>predictions>]
            4 [label = <<I>K </I> most similar<br/>resource <I>T<sub><font point-size='8'>R&apos;</font></sub></I>>]
            5 [label = <I(<I>T<sub><font point-size='8'>C</font></sub>,T<sub><font point-size='8'>R&apos;</font></sub></I>) in <I>C<SUB><font point-size='8'>R</font></SUB></I>?>]
            6 [label = <Add weight = <I>t(T<sub><font point-size='8'>R</font></sub>,T<sub><font point-size='8'>R&apos;</font></sub>,w<sub><font point-size='8'>t</font></sub>) </I><br/>to I(<I>T<sub><font point-size='8'>C</font></sub>,T<sub><font point-size='8'>R&apos;</font></sub></I>) in <I>C<SUB><font point-size='8'>R</font></SUB></I> if<br/><I>t </I> &gt; minimum threshold>]
            7 [label = <Add I(<I>T<sub><font point-size='8'>C</font></sub>,T<sub><font point-size='8'>R&apos;</font></sub></I>) to <I>C<SUB><font point-size='8'>R</font></SUB></I><br/>with weight = <I>t(T<sub><font point-size='8'>R</font></sub>,T<sub><font point-size='8'>R&apos;</font></sub>,w<sub><font point-size='8'>t</font></sub>) </I> if<br/><I>t </I> &gt; minimum threshold>]
            8 [label = <<I>K </I> most similar<br/>consumer <I>T<sub><font point-size='8'>C&apos;</font></sub></I>>]
            9 [label = <I(<I>T<sub><font point-size='8'>C&apos;</font></sub>,T<sub><font point-size='8'>R</font></sub></I>) in <I>S1</I>?>]
            10 [label = <I(<I>T<sub><font point-size='8'>C&apos;</font></sub>,T<sub><font point-size='8'>R</font></sub></I>) in <I>C<SUB><font point-size='8'>R</font></SUB></I>?>]
            11 [label = <Add 1 to I(<I>T<sub><font point-size='8'>C&apos;</font></sub>,T<sub><font point-size='8'>R</font></sub></I>)<br/>weight in <I>C<SUB><font point-size='8'>R</font></SUB></I>>]
            12 [label = <Add I(<I>T<sub><font point-size='8'>C&apos;</font></sub>,T<sub><font point-size='8'>R</font></sub></I>) to <I>C<SUB><font point-size='8'>R </font></SUB></I><br/>with weight = 1>]
            13 [label = <<I>K </I> most similar<br/>resource <I>T<sub><font point-size='8'>R&apos;</font></sub></I>>]
            14 [label = <I(<I>T<sub><font point-size='8'>C&apos;</font></sub>,T<sub><font point-size='8'>R&apos;</font></sub></I>) in <I>C<SUB><font point-size='8'>R</font></SUB></I>?>]
            15 [label = <Add weight = <I>t(T<sub><font point-size='8'>R</font></sub>,T<sub><font point-size='8'>R&apos;</font></sub>,w<sub><font point-size='8'>t</font></sub>) </I><br/>to I(<I>T<sub><font point-size='8'>C</font></sub>,T<sub><font point-size='8'>R&apos;</font></sub></I>) in <I>C<SUB><font point-size='8'>R</font></SUB></I> if<br/><I>t </I> &gt; minimum threshold>]
            16 [label = <Add I(<I>T<sub><font point-size='8'>C</font></sub>,T<sub><font point-size='8'>R&apos;</font></sub></I>) to <I>C<SUB><font point-size='8'>R</font></SUB></I><br/>with weight = <I>t(T<sub><font point-size='8'>R</font></sub>,T<sub><font point-size='8'>R&apos;</font></sub>,w<sub><font point-size='8'>t</font></sub>) </I> if<br/><I>t </I> &gt; minimum threshold>]
            17 [label = <Add I(<I>T<sub><font point-size='8'>C</font></sub>,T<sub><font point-size='8'>R&apos;</font></sub></I>) to predictions if weight &gt; minimum weight>]

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
1 -> 8 [label = 'No', headport = 'n', tailport = 'e']
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
