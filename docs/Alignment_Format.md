Docs - Table of Contents
====

 * Output formats
  * [**Alignment Format**](./Alignment_Format.md)
  * [Nodes and Edges Format](./Nodes_and_Edges_Format.md)
 * Data
  * [Hand Alignments](./Hand_Alignments.md)
 * Training
  * [Config File](./Config_File.md)
  * [Step by Step Training](./Step_by_Step_Training.md)
 * Evaluation
  * [Alignment Evaluation](./Alignment_Evaluation.md)
  * [Parser Performance](./Parser_Performance.md)
 * Development
  * [Developer's Guide](./Developers_Guide.md)

---

Alignment Format
===

This page describes the format of the alignments produced by the automatic aligner and by JAMR.

The alignments are in the `::alignments` field.  The alignments align spans of tokens in the `::tok` field with graph
fragments.  The format is a space separated list of spans with their graph fragments.  It looks like this:

    span1_start-span1_end|span1_node1+span1_node2+... span2_start-span2_end|span2_node1+span2_node2+...

Each node is specified by a descriptor (a [Gorn address](https://en.wikipedia.org/wiki/Gorn_address)): `0` for the root node, `0.0` for the first child of the root node, `0.1` for the
second child of the root node, etc.  (Our numbering system skips variable re-entrancies.)

So for the sentence:

    2002-01-05

and the AMR:

```
(d / date-entity
  :year 2002
  :month 1
  :day 5)
```

The alignments `0-1|0+0.0+0.1+0.2` align the first token (tokens 0 to 1, not including 1) to the graph fragment:

```
(d / date-entity
  :year 2002
  :month 1
  :day 5)
```

Another example:

```
# ::tok International ; military ; terrorism
# ::alignments 1-2|0 4-5|0.2 2-3|0.1 0-1|0.0
(a / and
  :op1 (i / international)
  :op2 (m / military)
  :op2 (t / terrorism))
```

Concept `and` is aligned to token 1 `;`
Concept `international` is aligned to token 0 `International`
etc.

Another example:

```
# ::tok Saudi Arabia ( SA )
# ::alignments 0-2|0+0.0+0.0.0+0.0.1
(c / country
  :name (n / name
          :op1 "Saudi"
          :op2 "Arabia"))
```

The graph fragment
```
(c / country
  :name (n / name
          :op1 "Saudi"
          :op2 "Arabia"))
```

is aligned to the tokens 0-2 `Saudi Arabia`.

The numbering system skips variable re-entrancies:

```
# ::tok North Korea has denied the IAEA full access to its facilities .
# ::alignments 0-2|0.0+0.0.0+0.0.0.0+0.0.0.1 5-6|0.2+0.2.0+0.2.0.0 3-4|0 7-8|0.1 6-7|0.1.1 10-11|0.1.0 ::annotator Aligner v.02 ::date 2014-08-08T20:42:25.346
(d / deny-01
  :ARG0 (c / country
          :name (n / name
                  :op1 "North"
                  :op2 "Korea"))
  :ARG1 (a / access-01
          :ARG0 o
          :ARG1 (f / facility
                  :poss c)
          :mod (f2 / full))
  :ARG2 (o / organization
          :name (n2 / name
                  :op1 "IAEA"))) 
```

So concept `facility` is `0.1.0` instead of `0.1.1`. 

