(use-modules (guix profiles)
			 (guix packages)
			 (guix transformations)
			 (gnu packages emacs)
			 (gnu packages xorg)
			 (gnu packages version-control)
			 (gnu packages cmake)
			 (gnu packages rust-apps)
			 (gnu packages compton)
			 (gnu packages linux)
			 (gnu packages screen)
			 (gnu packages image)
			 (gnu packages video)
			 (gnu packages time)
			 (gnu packages protobuf)
			 (gnu packages rpc)
			 (guix download)
			 (guix build-system python)
			 (gnu packages python-xyz)
			 (gnu packages python-web)
			 (gnu packages python-crypto)
			 (gnu packages python-build)
			 (gnu packages python-science)
			 (gnu packages python-compression)
			 (gnu packages tree-sitter)
			 (gnu packages libffi)
			 (gnu packages xdisorg)
			 (gnu packages audio)
			 (gnu packages databases)
			 (guix build-system pyproject))

(define-public k-emacs
  (package
   (inherit emacs-next)
   (version "31.0.50")
   (name "k-emacs")
   (inputs (modify-inputs (package-inputs emacs-next)
						  (prepend
						   libxrender
						   libxt)))))

(define-public python-imgcat
  (package
    (name "python-imgcat")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "imgcat" version))
       (sha256
        (base32 "0p4ksyy6zy3xz75l71c52ni69m7bji6803ir26vgkwdkmml7nm5k"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-matplotlib
                         python-numpy
                         python-pillow
                         python-pytest
                         python-setuptools
                         ;; python-tensorflow
                         ;; python-torch
                         ;; python-wheel
						 ))
    (home-page "https://github.com/wookayin/python-imgcat")
    (synopsis "imgcat as Python API and CLI")
    (description "imgcat as Python API and CLI.")
    (license #f)))

(define-public python-kiwisolver
  (package
    (name "python-kiwisolver")
    (version "1.4.8")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "kiwisolver" version))
       (sha256
        (base32 "0zlq6x9g98c85021lzsdmhg60nxjbdfwl0sznr7fbiy8pliz1m93"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-cppy python-setuptools python-setuptools-scm
                         python-wheel))
    (home-page "null")
    (synopsis "A fast implementation of the Cassowary constraint solver")
    (description
     "This package provides a fast implementation of the Cassowary constraint solver.")
    (license #f)))

(define-public python-lox
  (package
    (name "python-lox")
    (version "0.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "lox" version))
       (sha256
        (base32 "0d5v25r5n3z5hm1sdh6fxj6any7vw1vj8y3fbvn2xsca3ph1fyzj"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-pathos))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/BrianPugh/lox")
    (synopsis "Threading and Multiprocessing for every project.")
    (description "Threading and Multiprocessing for every project.")
    (license #f)))

(define-public python-pip-tools
  (package
    (name "python-pip-tools")
    (version "7.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pip-tools" version))
       (sha256
        (base32 "1jdw2hgm4yskpygdd8s8k2qdy34jwdfbivnv4h74ar1q0zsjcj46"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-build
                             python-click
                             ;; python-django
                             python-pip
                             python-pyproject-hooks
                             python-setuptools
                             python-wheel))
    (native-inputs (list python-flit-core
                         python-hatchling
                         python-poetry-core
                         python-pytest
                         python-pytest-rerunfailures
                         python-pytest-xdist
                         python-tomli-w))
    (home-page "")
    (synopsis "pip-tools keeps your pinned dependencies fresh.")
    (description "pip-tools keeps your pinned dependencies fresh.")
    (license #f)))

(define-public python-pytest
  (package
    (name "python-pytest")
    (version "8.3.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest" version))
       (sha256
        (base32 "0i9q7lkg43h8y4m233zl25dqaap1g715fss7mijialafq46fgvzl"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-colorama
                             python-exceptiongroup
                             python-iniconfig
                             python-packaging
                             python-pluggy
                             python-tomli))
    (native-inputs (list python-argcomplete
                         python-attrs
                         ;; python-hypothesis
                         ;; python-mock
                         ;; python-pygments
                         ;; python-requests
                         ;; python-setuptools
                         ;; python-setuptools-scm
                         ;; python-wheel
                         ;; python-xmlschema
						 ))
    (home-page "null")
    (synopsis "pytest: simple powerful testing with Python")
    (description "pytest: simple powerful testing with Python.")
    (license #f)))

(define-public python-pytest-env
  (package
    (name "python-pytest-env")
    (version "1.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest_env" version))
       (sha256
        (base32 "1ky11hgb00wdz3mrsfk6zp17r56j99allimcfd83hhqfm909h84i"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-pytest python-tomli))
    (native-inputs (list python-covdefaults python-coverage python-hatch-vcs
                         python-hatchling python-pytest-mock))
    (home-page "null")
    (synopsis "pytest plugin that allows you to add environment variables.")
    (description "pytest plugin that allows you to add environment variables.")
    (license #f)))

(define-public python-uv
  (package
    (name "python-uv")
    (version "0.0.38")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "python_uv" version))
       (sha256
        (base32 "1i0q63zdzvg4b00xfw03rlxxny8ssqcqmxnx5i2f4zdv526qljfz"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-hatchling))
    (home-page "null")
    (synopsis "python template package")
    (description "python template package.")
    (license #f)))



(define-public python-backoff
  (package
    (name "python-backoff")
    (version "2.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "backoff" version))
       (sha256
        (base32 "1fjwz9x81wpfn22j96ck49l3nb2hn19qfgv44441h8qrpgsjky03"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-poetry-core))
    (home-page "https://github.com/litl/backoff")
    (synopsis "Function decoration for backoff and retry")
    (description "Function decoration for backoff and retry.")
    (license #f)))

(define-public python-resend
  (package
    (name "python-resend")
    (version "2.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "resend" version))
       (sha256
        (base32 "11011xaqkc48hnsq1nkd1afbgbmn0q0y07ks0qmd1sqzfq5j2577"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-requests python-typing-extensions))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/resendlabs/resend-python")
    (synopsis "Resend Python SDK")
    (description "Resend Python SDK.")
    (license #f)))

(define-public python-msal-extensions
  (package
    (name "python-msal-extensions")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "msal_extensions" version))
       (sha256
        (base32 "1vvbckn78n3xj7750bqspn27vrnk1p592p1139ikv4yjpwhb6hbg"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-msal python-portalocker))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "null")
    (synopsis
     "Microsoft Authentication Library extensions (MSAL EX) provides a persistence API that can save your data on disk, encrypted on Windows, macOS and Linux. Concurrent data access will be coordinated by a file lock mechanism.")
    (description
     "Microsoft Authentication Library extensions (MSAL EX) provides a persistence API
that can save your data on disk, encrypted on Windows, @code{macOS} and Linux.
Concurrent data access will be coordinated by a file lock mechanism.")
    (license #f)))


(define-public python-azure-identity
  (package
    (name "python-azure-identity")
    (version "1.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "azure_identity" version))
       (sha256
        (base32 "05503gd1a8r9pfyj0gkz3sg1h834naijxzmh658f30vcsl874na0"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-azure-core python-cryptography python-msal
                             python-msal-extensions python-typing-extensions))
    (home-page
     "https://github.com/Azure/azure-sdk-for-python/tree/main/sdk/identity/azure-identity")
    (synopsis "Microsoft Azure Identity Library for Python")
    (description "Microsoft Azure Identity Library for Python.")
    (license #f)))

(define-public python-google-cloud-kms
  (package
    (name "python-google-cloud-kms")
    (version "3.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "google_cloud_kms" version))
       (sha256
        (base32 "1hj86iv8izij1lzhi8h9j1720z4ymp204mn2m1d9537z6p5klah4"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-google-api-core python-google-auth
                             python-grpc-google-iam-v1 python-proto-plus
                             python-protobuf))
    (native-inputs (list python-setuptools python-wheel))
    (home-page
     "https://github.com/googleapis/google-cloud-python/tree/main/packages/google-cloud-kms")
    (synopsis "Google Cloud Kms API client library")
    (description "Google Cloud Kms API client library.")
    (license #f)))

(define-public python-grpc-google-iam-v1
  (package
    (name "python-grpc-google-iam-v1")
    (version "0.14.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "grpc_google_iam_v1" version))
       (sha256
        (base32 "14d8z2dmhvh5lmljx74x8y59w9k3hrcaxa12ibx7jmqfmwvry50l"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-googleapis-common-protos python-grpcio
                             python-protobuf))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "null")
    (synopsis "IAM API client library")
    (description "IAM API client library.")
    (license #f)))

(define-public python-gunicorn
  (package
    (name "python-gunicorn")
    (version "23.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "gunicorn" version))
       (sha256
        (base32 "1v04l35k103289h5inwv1v4s89q2njk8rhgnjki5gp0105x4857h"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-importlib-metadata python-packaging))
    (native-inputs (list python-coverage
                         python-eventlet
                         python-gevent
                         python-pytest
                         python-pytest-cov
                         python-setuptools
                         python-wheel))
    (home-page "null")
    (synopsis "WSGI HTTP Server for UNIX")
    (description "WSGI HTTP Server for UNIX.")
    (license #f)))

(define-public python-fastapi-sso
  (package
    (name "python-fastapi-sso")
    (version "0.17.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "fastapi_sso" version))
       (sha256
        (base32 "09y6fn9429mir01xfpnr4g805vzl4lcrqrkli2lrx69mm6i4c3ps"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-fastapi python-httpx python-oauthlib
                             python-pydantic python-typing-extensions))
    (native-inputs (list python-poetry-core))
    (home-page "https://tomasvotava.github.io/fastapi-sso/")
    (synopsis
     "FastAPI plugin to enable SSO to most common providers (such as Facebook login, Google login and login via Microsoft Office 365 Account)")
    (description
     "@code{FastAPI} plugin to enable SSO to most common providers (such as Facebook
login, Google login and login via Microsoft Office 365 Account).")
    (license #f)))

(define-public python-azure-keyvault-secrets
  (package
    (name "python-azure-keyvault-secrets")
    (version "4.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "azure_keyvault_secrets" version))
       (sha256
        (base32 "1qczi2jp5zbqwgn0frp0kmd9h4q1678djc63s656q3cszlpvn0ra"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-azure-core python-isodate
                             python-typing-extensions))
    (native-inputs (list python-setuptools python-wheel))
    (home-page
     "https://github.com/Azure/azure-sdk-for-python/tree/main/sdk/keyvault/azure-keyvault-secrets")
    (synopsis "Microsoft Azure Key Vault Secrets Client Library for Python")
    (description
     "Microsoft Azure Key Vault Secrets Client Library for Python.")
    (license #f)))

(define-public python-tiktoken
  (package
    (name "python-tiktoken")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "tiktoken" version))
       (sha256
        (base32 "0p9cg6n8mzdi4lbbwxrrp26chy5hr14bqmzr3w74kq1qm6k5qanh"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-regex python-requests))
    (native-inputs (list python-setuptools python-setuptools-rust python-wheel))
    (home-page "null")
    (synopsis "tiktoken is a fast BPE tokeniser for use with OpenAI's models")
    (description
     "tiktoken is a fast BPE tokeniser for use with @code{OpenAI's} models.")
    (license #f)))

(define-public python-tokenizers
  (package
    (name "python-tokenizers")
    (version "0.21.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "tokenizers" version))
       (sha256
        (base32 "1r2ghgifmq9j85d3zsflfpb388sbmrckiwvr630b0x8v66zr827f"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-huggingface-hub python-huggingface-hub))
    (native-inputs (list python-black
                         python-datasets
                         python-maturin
                         python-numpy
                         python-pytest
                         python-requests
                         python-ruff
                         python-tokenizers))
    (home-page "null")
    (synopsis "null")
    (description #f)
    (license #f)))

(define-public python-tree-sitter-c-sharp
  (package
    (name "python-tree-sitter-c-sharp")
    (version "0.23.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "tree_sitter_c_sharp" version))
       (sha256
        (base32 "1sv7r52zghzyc1n2y25cmrc69yim6gm2lsr7fl1q8yjl7byjqbij"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-setuptools python-wheel))
    (home-page "null")
    (synopsis "C# grammar for tree-sitter")
    (description "C# grammar for tree-sitter.")
    (license #f)))

(define-public python-tree-sitter-embedded-template
  (package
    (name "python-tree-sitter-embedded-template")
    (version "0.23.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "tree_sitter_embedded_template" version))
       (sha256
        (base32 "1anxvh81aijvnjvxn6dppyl30ql66r6mc5z64d1zb5r4x7rdq93v"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-setuptools python-wheel))
    (home-page "null")
    (synopsis "Embedded Template (ERB, EJS) grammar for tree-sitter")
    (description "Embedded Template (ERB, EJS) grammar for tree-sitter.")
    (license #f)))

(define-public python-tree-sitter-language-pack
  (package
    (name "python-tree-sitter-language-pack")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "tree_sitter_language_pack" version))
       (sha256
        (base32 "09jv2nlmrgl0b35l23nrcrysvxikid6qkhqhd9mafa277k44fkaa"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-tree-sitter python-tree-sitter-c-sharp
                             python-tree-sitter-embedded-template
                             python-tree-sitter-yaml))
    (native-inputs (list python-cython python-setuptools
                         python-typing-extensions python-wheel))
    (home-page "null")
    (synopsis "Extensive Language Pack for Tree-Sitter")
    (description "Extensive Language Pack for Tree-Sitter.")
    (license #f)))

(define-public python-tree-sitter-yaml
  (package
    (name "python-tree-sitter-yaml")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "tree_sitter_yaml" version))
       (sha256
        (base32 "191mhjzsifh5y9s5v6mmscy8h68d1hj1f296azkv1hsmjxyv32ww"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-setuptools python-wheel))
    (home-page "null")
    (synopsis "YAML grammar for tree-sitter")
    (description "YAML grammar for tree-sitter.")
    (license #f)))

(define-public python-watchfiles
  (package
    (name "python-watchfiles")
    (version "1.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "watchfiles" version))
       (sha256
        (base32 "01bjf55bv3fsnpjrypydfgfip72z4lqbghh09wzdfqhhs7pp793b"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-anyio))
    (native-inputs (list python-maturin))
    (home-page "https://github.com/samuelcolvin/watchfiles")
    (synopsis
     "Simple, modern and high performance file watching and code reload in python.")
    (description
     "Simple, modern and high performance file watching and code reload in python.")
    (license #f)))

(define-public python-grep-ast
  (package
    (name "python-grep-ast")
    (version "0.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "grep_ast" version))
       (sha256
        (base32 "05g1lkzrc7fy57r3ghs5djxb4381cpa0m4mk93a6gp16yn2vipg2"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-pathspec python-tree-sitter-language-pack))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/paul-gauthier/grep-ast")
    (synopsis "A tool to grep through the AST of a source file")
    (description
     "This package provides a tool to grep through the AST of a source file.")
    (license #f)))

(define-public python-posthog
  (package
    (name "python-posthog")
    (version "3.19.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "posthog" version))
       (sha256
        (base32 "1g1994f661hf47pp00rwdxc25fh97ldannslm68ym1z2gljvqydq"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-backoff
                             python-distro
                             python-monotonic
                             python-dateutil
                             python-requests
                             python-six))
    (native-inputs (list python-anthropic
                         python-black
                         python-coverage
                         ;; python-django
                         ;; python-django-stubs
                         python-flake8
                         python-flake8-print
                         python-freezegun
                         python-isort
                         python-langchain-anthropic
                         python-langchain-community
                         python-langchain-openai
                         python-langgraph
                         python-lxml
                         python-mock
                         python-mypy
                         python-mypy-baseline
                         python-openai
                         python-parameterized
                         python-pre-commit
                         python-pydantic
                         python-pylint
                         python-pytest
                         python-pytest-asyncio
                         python-pytest-timeout
                         python-types-mock
                         python-types-python-dateutil
                         python-types-requests
                         python-types-setuptools
                         python-types-six))
    (home-page "https://github.com/posthog/posthog-python")
    (synopsis "Integrate PostHog into any python application.")
    (description "Integrate @code{PostHog} into any python application.")
    (license #f)))


(define-public python-jiter
  (package
    (name "python-jiter")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jiter" version))
       (sha256
        (base32 "14vqkmiwgb7ivfp0bqpy3fiw8q4w48yvqaj94jm4shpb9nba1nxa"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-maturin))
    (home-page "https://github.com/pydantic/jiter/")
    (synopsis "Fast iterable JSON parser.")
    (description "Fast iterable JSON parser.")
    (license #f)))

(define-public python-litellm
  (package
    (name "python-litellm")
    (version "1.63.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "litellm" version))
       (sha256
        (base32 "1bmrbxz2jw5m1jg2jw4ch8mg98pinn4hda9q3w8d3xqp3z56xzdq"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-aiohttp
                             python-apscheduler
                             python-azure-identity
                             python-azure-keyvault-secrets
                             python-backoff
                             python-click
                             python-cryptography
                             python-fastapi
                             python-fastapi-sso
                             python-google-cloud-kms
                             python-gunicorn
                             python-httpx
                             python-importlib-metadata
                             python-jinja2
                             python-jsonschema
                             python-openai
                             python-orjson
                             python-prisma
                             python-pydantic
                             python-pyjwt
                             python-pynacl
                             python-dotenv
                             python-multipart
                             python-pyyaml
                             python-resend
                             python-rq
                             python-tiktoken
                             python-tokenizers
                             python-uvicorn
                             python-uvloop))
    (native-inputs (list python-poetry-core python-wheel))
    (home-page "null")
    (synopsis "Library to easily interface with LLM API providers")
    (description "Library to easily interface with LLM API providers.")
    (license #f)))

(define-public python-mixpanel
  (package
    (name "python-mixpanel")
    (version "4.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "mixpanel" version))
       (sha256
        (base32 "05d101vssnlsrr8lb2aqdc6jidp72qflx7r4iv7hakyk7mvvb9i9"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-requests python-six python-urllib3))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/mixpanel/mixpanel-python")
    (synopsis "Official Mixpanel library for Python")
    (description "Official Mixpanel library for Python.")
	(license #f)))

(define-public python-sounddevice
  (package
    (name "python-sounddevice")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sounddevice" version))
       (sha256
        (base32 "0hdhdk0s07hpr54a75dnlvpq33qwm2lia7n9kazf936smqfrkjh9"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-cffi))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "http://python-sounddevice.readthedocs.io/")
    (synopsis "Play and Record Sound with Python")
    (description "Play and Record Sound with Python.")
    (license #f)))


(define-public python-aider-chat
	  (package
       (name "python-aider-chat")
       (version "0.76.1")
       (source
		(origin
		 (method url-fetch)
		 (uri (pypi-uri "aider_chat" version))
		 (sha256
          (base32 "033abyqbwiyigjm113w1vyr74zrc7sbaa8d2653q4crhgw8b5kzl"))))
       (build-system pyproject-build-system)
       (propagated-inputs (list python-aiohappyeyeballs
								;; python-aiohttp
								;; python-aiosignal
								;; python-annotated-types
								;; python-anyio
								;; python-attrs
								;; python-backoff
								;; python-beautifulsoup4
								;; python-certifi
								;; python-cffi
								;; python-charset-normalizer
								;; python-click
								;; python-configargparse
								;; python-diff-match-patch
								;; python-diskcache
								;; python-distro
								;; python-filelock
								;; python-flake8
								;; python-frozenlist
								;; python-fsspec
								;; python-gitdb
								;; python-gitpython
								;; python-grep-ast
								;; python-h11
								;; python-httpcore
								;; python-httpx
								;; python-huggingface-hub
								;; python-idna
								;; python-importlib-metadata
								;; python-importlib-resources
								;; python-jinja2
								;; python-jiter
								;; python-json5
								;; python-jsonschema
								;; python-jsonschema-specifications
								;; python-litellm
								;; python-markdown-it-py
								;; python-markupsafe
								;; python-mccabe
								;; python-mdurl
								;; python-mixpanel
								;; python-monotonic
								;; python-multidict
								;; python-networkx
								;; python-numpy
								;; python-openai
								;; python-packaging
								;; python-pathspec
								;; python-pexpect
								;; python-pillow
								;; python-pip
								;; python-posthog
								;; python-prompt-toolkit
								;; python-propcache
								;; python-psutil
								;; python-ptyprocess
								;; python-pycodestyle
								;; python-pycparser
								;; python-pydantic
								;; python-pydantic-core
								;; python-pydub
								;; python-pyflakes
								;; python-pygments
								;; python-pypandoc
								;; python-pyperclip
								;; python-dateutil
								;; python-dotenv
								;; python-pyyaml
								;; python-referencing
								;; python-regex
								;; python-requests
								;; python-rich
								;; python-rpds-py
								;; python-scipy
								;; python-six
								;; python-smmap
								;; python-sniffio
								;; python-socksio
								;; python-sounddevice
								;; python-soundfile
								;; python-soupsieve
								;; python-tiktoken
								;; python-tokenizers
								;; python-tqdm
								;; python-tree-sitter-c-sharp
								;; python-tree-sitter-embedded-template
								;; python-tree-sitter-language-pack
								;; python-tree-sitter-yaml
								;; python-typing-extensions
								;; python-urllib3
								;; python-watchfiles
								;; python-wcwidth
								python-yarl
								python-zipp))
       (native-inputs (list python-build
							;; python-cfgv
							;; python-click
							;; python-codespell
							;; python-cogapp
							;; python-contourpy
							;; python-cycler
							;; python-dill
							;; python-distlib
							;; python-filelock
							;; python-fonttools
							;; python-identify
							;; python-imgcat
							;; python-iniconfig
							;; python-kiwisolver
							;; python-lox
							;; python-markdown-it-py
							;; python-matplotlib
							;; python-mdurl
							;; python-multiprocess
							;; python-nodeenv
							;; python-numpy
							;; python-packaging
							;; python-pandas
							;; python-pathos
							;; python-pillow
							;; python-pip
							;; python-pip-tools
							;; python-platformdirs
							;; python-pluggy
							;; python-pox
							;; python-ppft
							;; python-pre-commit
							;; python-pygments
							;; python-pyparsing
							;; python-pyproject-hooks
							;; python-pytest
							;; python-pytest-env
							;; python-dateutil
							;; python-pytz
							;; python-pyyaml
							;; python-rich
							;; python-semver
							;; python-setuptools
							;; python-setuptools-scm
							;; python-shellingham
							;; python-six
							;; python-typer
							;; python-typing-extensions
							;; python-tzdata
							;; python-uv
							;; python-virtualenv
							python-wheel))
       (home-page "null")
       (synopsis "Aider is AI pair programming in your terminal")
       (description "Aider is AI pair programming in your terminal.")
       (license #f)))

(packages->manifest (list git
						  cmake
						  k-emacs
						  ripgrep
						  picom
						  git-delta
						  brightnessctl
						  dtach
						  ;; nushell
						  flameshot
						  mpv
						  clojure
						  openjdk
						  ;; python-aider-chat
						  ))
