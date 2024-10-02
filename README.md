# AIworkflow Package for R

This package, **aiworkflow**, is aimed at making it simple to interact with local LLMs when using R.
This package is NOT aimed at creating chat clients (while it could definitely support such a use case) but rather executing LLMs over a large amount of data, in a reproducible way. Think about executing LLMs NLP tasks on dataframes as a typical use case.

This package is very much in *alpha* stages. It can already do a lot of things, but it lacks complete testing coverage, and the API of the package is subject to change. You should be aware that functions may change and be deprecated until the 1.0 version is reached. 

## Installation

You can use install the github version directly with devtools:

```
devtools::install_github("r-guyot/aiworkflow")
```

or pak:

```
pak::pkg_install("r-guyot/aiworkflow")
```

There is currently no CRAN package but this may change in the near future.

## License

LGPL v3, which means, in layman terms:

- you have to share and redistribute any modification you make to this package
- you are free to use this package as-is to power your own applications, whether they are Open-source or proprietary. You do not have to release the code of your proprietary apps that use this package.

Please refer to the full details of the LICENSE document in any case.

## Requirements

You need to have at least:

- an Ollama instance running on your machine, along with an embedding model and a LLM already downloaded on it.

to be able to use this package at the moment. A qdrant instance is optional.

## Current Features

In its current version it brings the following features:


- pipes support for LLM operations
- client for Ollama to run local LLM operations
- client for Qdrant database to store vector embeddings
- support for basic RAG
- support for tool calling for LLMs that support it (like Llama3.1)
- support for local vector embeddings database using a feather file
- numerous processing skills (pre-defined prompts) that can be used out of the box
- support for chaining multiple LLM operations in pipes
- support for numerous prompt modification functions (audience, role, style, etc...)
- support for JSON output extraction
- and probably some more...


## Upcoming Features

### CRAN

The goal is to have this published on CRAN once this is robust enough and in a more complete shape.

### LLM Backend support 

Ultimately the idea of this package is to expand to more backends to run LLMs:


- llama.cpp
- VLLM
- llamafile


### Vision LLM Support

This package will also support Vision models that accept images as inputs, down the road.

### Image Generation Support

This package will eventually also support image generation through the ComfyUI API (most likely).

## Contributions

If you are interested to contribute to this package, you are welcome to issue a PR. 
Please also consider filing requests for new features and of course bug reports.


