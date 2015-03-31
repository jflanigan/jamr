Docs - Table of Contents
====

 * Output formats
  * [Alignment Format](./Alignment_Format.md)
  * [Nodes and Edges Format](./Nodes_and_Edges_Format.md)
 * Data
  * [Hand Alignments](./Hand_Alignments.md)
 * Training
  * [**Config File**](./Config_File.md)
  * [Step by Step Training](./Step_by_Step_Training.md)
 * Evaluation
  * [Alignment Evaluation](./Alignment_Evaluation.md)
  * [Parser Performance](./Parser_Performance.md)
 * Development
  * [Developer's Guide](./Developers_Guide.md)

---

Config File
==============

The config file sets the environment variables for JAMR.  The variables are:

| Environment Variable | Purpose |
| --- | --- |
| `MODEL_DIR` | Absolute path to the directory to save the model to |
| `TRAIN_FILE` | Absolute path to training file |
| `DEV_FILE` | Absolute path to dev file (evaluated on after each pass during training) |
| `TEST_FILE` | Absolute path to testing file (evaluated at end of training) |
| `PARSER_OPTIONS` | Options to the parser (also used by `PARSE.sh`) |
| `CONCEPT_ID_TRAINING_OPTIONS` | Options for concept ID training |
| `RELATION_ID_TRAINING_OPTIONS` | Options for relation ID training |

There are also variable for the paths to the external depdendencies.  For the list of possible `PARSER_OPTIONS`, see `src/AMRParser.scala`.

