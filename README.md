JAMR - AMR Parser
=================

JAMR is a semantic parser and aligner for the [Abstract Meaning
Representation](http://amr.isi.edu/). 

We have also released [hand-alignments](docs/Hand_Alignments.md) for 200 sentences of the AMR corpus.

#Building

First checkout the github repository:

    git clone https://github.com/jflanigan/jamr.git

JAMR depends on [Scala](http://www.scala-lang.org), [Illinois NER
system](http://cogcomp.cs.illinois.edu/page/download_view/NETagger) v2.7,
tokenization scripts in [cdec](https://github.com/redpony/cdec),
and [WordNet](http://wordnetcode.princeton.edu/3.0/WordNet-3.0.tar.gz) (for the
aligner). To download these dependencies into the subdirectory `tools`,
cd to the `jamr` repository and run

    ./setup

You should agree to the terms and conditions of the software dependencies
before running this script.  If you download them yourself, you will need to change
the relevant environment variables in `scripts/config.sh`.

Source the config script - you will need to do this before running any of the scripts below:

    . scripts/config.sh

Run `./compile` to build an uberjar, which will be output to
    `target/scala-{scala_version}/jamr-assembly-{jamr_version}.jar`
If you get out of memory errors during this step, you may need to edit the Java memory options
in the script `sbt` and `build.sbt`.

#Running the Parser

Download and extract the model weights [current.tgz](http://cs.cmu.edu/~jmflanig/current.tgz) into the directory `$JAMR_HOME/experiments/current`.  To parse a file (cased, untokenized, with one sentence per line):

    . scripts/config.sh
    scripts/PARSE.sh < input_file > output_file 2> output_file.err

The output is AMR format, with some extra fields described in [docs/Nodes and Edges Format](docs/Nodes_and_Edges_Format.md) and [docs/Alignment Format](docs/Alignment_Format.md).

#Running the Aligner

To run the rule-based aligner:

    . scripts/config.sh
    scripts/ALIGN.sh < amr_input_file > output_file

The output of the aligner is described in [docs/Alignment Format](docs/Alignment_Format.md).  Currently the aligner works best for release r3 data (AMR Specification v1.0), but it will run on newer data as well.

#Hand Alignments

To create the hand alignments file, see [docs/Hand Alignments](docs/Hand_Alignments.md).

#Experimental Pipeline

The following steps describe how to train and evaluate the parser on the release r3
data, using the same train/dev/test splits as in the paper.  The script
`./train` will do all these steps for you, or you can do them step-by-step below.

To have the `./train` script do all these steps for you, first download
`LDC2013E117.tgz` from the LDC Catalog (requires an LDC subscription) into the directory `data/`.
Then run

    ./train

It should take about 3-6 hours to train.  At the end it will report the
performance of the parser.

##1. Preprocessing the data

Download `LDC2013E117.tgz` from the LDC Catalog.
Extract the file `deft-amr-release-r3-proxy.txt` into `data/LDC-2013-Sep/` and rename it
`amr-release-proxy.txt`.

    cd scripts/preprocessing

Run `./PREPROCESS.sh`.


##2. Training

(To skip this step, which takes about 3-6 hours, download and extract model
weights [current.tgz](http://cs.cmu.edu/~jmflanig/current.tgz) into the
directory $JAMR_HOME/experiments/current.)

    cd scripts/training

Extract concept table:

    ./cmd.conceptTable.train

Concept identification (stage1) training:

    ./cmd.stage1-weights

Relation identification (stage2) training:

    ./cmd.stage2-weights

Search for 'Performance on Dev' in `stage2-weights.err` for early stopping.


##3. Evaluating

Decode test set:

    ./cmd.test.decode.allstages

  or

    ./cmd.test.decode.stage2only

Evaluate the predictions using smatch:

    ${JAMR_HOME}/scripts/smatch_v1_0/smatch_modified.py --pr -f ${JAMR_HOME}/experiments/current/test.decode.allstages ${JAMR_HOME}/data/LDC-2013-Sep/amr-release-proxy.test
    ${JAMR_HOME}/scripts/smatch_v1_0/smatch_modified.py --pr -f ${JAMR_HOME}/experiments/current/test.decode.stage2only ${JAMR_HOME}/data/LDC-2013-Sep/amr-release-proxy.test

