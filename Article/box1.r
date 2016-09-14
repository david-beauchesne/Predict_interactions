S0 <- matrix(nrow = 12, ncol = 4, data = NA, dimnames = list(c(),c('taxon','taxonomy','resource','consumer')))
S0[1, ] <- c('1', 'a | b | c', '2 | 3 | 12', '4')
S0[2, ] <- c('2', 'e | f | g', '', '1 | 5')
S0[3, ] <- c('3', 'i | j | k', '', '5')
S0[4, ] <- c('4', 'm | n | o', '1 | 5', '')
S0[5, ] <- c('5', 'a | b | d', '8 | 9', '4')
S0[6, ] <- c('6', 'i | q | r', '2 | 8', '4')
S0[7, ] <- c('7', 'e | f | h', '', '1 | 6')
S0[8, ] <- c('8', 's | t | u', '', '5 | 6')
S0[9, ] <- c('9', 's | t | v', '', '5')
S0[10, ] <- c('10', 'i | j | l', '', '')
S0[11, ] <- c('11', 'm | n | p', '', '')
S0[12, ] <- c('12', 'q | r | s', '', '1')
rownames(S0) <- S0[,'taxon']

S1 <- c('1','9','10','11','12')

example0 <- full_algorithm(Kc = 2, Kr = 2, S0 = S0, S1 = S1, MW = 0, wt = 0, minimum_threshold = 0)
example05 <- full_algorithm(Kc = 2, Kr = 2, S0 = S0, S1 = S1, MW = 0, wt = 0.5, minimum_threshold = 0)
example1 <- full_algorithm(Kc = 2, Kr = 2, S0 = S0, S1 = S1, MW = 0, wt = 1, minimum_threshold = 0)

example0
example05
example1

cons0 <- similarity_taxon(S0 = S0, wt = 0, taxa = 'consumer')
res0 <- similarity_taxon(S0 = S0, wt = 0, taxa = 'resource')
taxo <- similarity_taxon(S0 = S0, wt = 1, taxa = 'consumer')

sim.example <- matrix(nrow = nrow(cons0), ncol = ncol(cons0))
sim.example[upper.tri(sim.example)] <- cons0[upper.tri(cons0)]
sim.example[lower.tri(sim.example)] <- taxo[lower.tri(taxo)]
diag(sim.example) <- S0[, 'taxon']


library(DiagrammeR)
grViz("

digraph boxes_and_circles{

    node [shape = box
            fixedsize = TRUE
            width = 2]
    1 [label = <<I>T<SUB>1</SUB></I>>]
    2 [label = <<I>T<SUB>9</SUB></I>>]
    3 [label = <<I>T<SUB>10</SUB></I>>]
    4 [label = <<I>T<SUB>11</SUB></I>>]
    5 [label = <<I>T<SUB>12</SUB></I>>]

1 -> 2
1 -> 5

}
")

grViz("
digraph boxes_and_circles{

    node [shape = box
            fixedsize = TRUE
            width = 0.2
            fontsize = 9
            color = white]
    6 [label = <<I>T<SUB>1</SUB></I>>]
    7 [label = <<I>T<SUB>9</SUB></I>>]
    8 [label = <<I>T<SUB>10</SUB></I>>]
    9 [label = <<I>T<SUB>11</SUB></I>>]
    10 [label = <<I>T<SUB>12</SUB></I>>]

    1 [label = <<I>T<SUB>1</SUB></I>>]
    2 [label = <<I>T<SUB>9</SUB></I>>]
    3 [label = <<I>T<SUB>10</SUB></I>>]
    4 [label = <<I>T<SUB>11</SUB></I>>]
    5 [label = <<I>T<SUB>12</SUB></I>>]


    1 -> 2 [arrowsize = 0.5]
    1 -> 3 [arrowsize = 0.5]
    1 -> 5 [arrowsize = 0.5]
    3 -> 2 [arrowsize = 0.5]
    3 -> 5 [arrowsize = 0.5]
    4 -> 1 [arrowsize = 0.5]
    5 -> 2 [arrowsize = 0.5]

    6 -> 7 [arrowsize = 0.5]
    6 -> 8 [color = 'white'; arrowsize = 0]
    6 -> 10 [arrowsize = 0.5]
    8 -> 7 [color = 'white'; arrowsize = 0]
    8 -> 10 [color = 'white'; arrowsize = 0]
    9 -> 6 [color = 'white'; arrowsize = 0]
    10 -> 7 [color = 'white'; arrowsize = 0]


    graph [ranksep = 0.15
            rank = source
            # rankdir = LR
            ]
}
")
