# Jose Fernandes
**Rust Systems Software Engineer**

+44 7759 669644 · jf94.uk@gmail.com · linkedin.com/in/xecarlox94 · github.com/xecarlox94

---
## Professional Summary

Systems software engineer specialising in software engineering and architecture of real-time, safety-critical Rust applications for industrial and critical infrastructure. Experienced in designing actor-model concurrent systems with Rust, integrating industrial protocols, and building reliable software for environments where correctness and resilience matter. Track record of leading multi-disciplinary engineering and research teams to deliver measurable outcomes for major industrial clients. Particularly interested in critical systems, verified software, and the intersection of robust engineering with complex operational domains.

- v2

Rust systems software engineer with 1-2 years of professional Rust experience designing and building concurrent, safety-critical distributed systems. Specialist in actor-model architecture, async/await concurrency, and industrial communication protocols (Protobuf, HTTP/REST) for critical infrastructure and energy systems. Proven track record leading multi-disciplinary engineering and research teams to deliver measurable production outcomes. Additional experience in computer vision, real-time robotics, and cloud data engineering.

<TODO: Business-driven yet curious for cutting edge programming language and software verification research to offer competitive advantage to employer/client>
<TODO: finish this section>

---
## Skills

**Rust:** async/await, tokio (channels, select!, cancellation tokens, task trackers), sqlx, actor model, software testing

**Languages:** Rust, Haskell, Python, Ocaml/Rocq, Java/Scala, Nix, C/C++, C#/F#, Terraform

**Systems:** Linux, NixOS, Docker, RaspberryPi GPIO, Nvidia Jetson, WebAssembly, Protobuf, Open Telemetry, HTTP/REST, WebSockets

**Databases:** PostgreSQL, Azure SQL, NoSQL

**Cloud:** Azure (Synapse, Functions, Data Lake, Event Hub), AWS (S3, EC2), GCP (DataProc, BigQuery)

**Also:** Type driven development; Rust smart contract integration (sui and cosmos sdks);

---
## Experience

### Rust Software Engineer · Enoda Ltd · UK
**Mar 2025 – Present**

Part of the ENSEMBLE division building the aggregator: a market- and hardware-agnostic platform that bids grid capacity in real time across TSO regions and metering device vendors, supporting the transition to renewable energy infrastructure (financialising energy markets)

Architected and lead Rust engineer on the ENSEMBLE aggregator platform — a market- and hardware-agnostic system for real-time bidding of grid capacity across TSO regions and metering device vendors, supporting the transition to renewable energy critical infrastructure.

Influenced the technical and business aspects of the application aligning the development of a new platform with global ambitions

new platform, built its own primitives

<TODO: Functorial/Monadic layering to stricly segment resposibilities across the framework layers>

<TODO: finish header section for cv>


- Designed and implemented an actor-model application in Rust on tokio, comprising: a Core actor for bid aggregation and financial calculations with PostgreSQL persistence via sqlx; a Device actor for metering device communication over protobuf; and a WIRE actor for TSO grid command interfaces over HTTP/REST.
<TODO: explain that the actor-model architecture is matching the functional requirements allowing effective tracking of current (and future changes) to specification>

- Architected the system for vendor and market agnosticism: developed an SDK abstraction layer supporting multiple hardware vendors, and a pluggable market adapter pattern for different regional energy market APIs and different kinds of devices.
<TODO: deliberate usage of actor architecture to unify business functional requirements (and their updates)>
<TODO: embedding business logic into type system to track functional requirements and their updates>
<TODO: layered architecture that handles different responsibilities>
<TODO: typestate pattern to enforce algebraic-driven parsing and business-level validation>
<TODO: maybe merge this with bullet point above?>

- Engineered robust IO-bounded concurrency model, leveraging tokio primitives, with fault-tolerant supervision of running instances and their internal resources;

- Installed a monitoring and alerting layer, leveraging open-telemetry, to trace application and business operations, and react to certain kinds of runtime errors;

- Built comprehensive unit testing coverage across framework's components and functional requirements driven integration testing and aligned with the actor-model;

- Currently in integration testing with an European TSO;

---
### Software Engineer (Project Lead) · National Robotarium · Edinburgh, UK
**Nov 2022 – Mar 2025**

Worked across a portfolio of industrial production-driven AI and Robotics R&D projects for major UK industrial clients, progressing to project lead engineer;

- Led a multi-disciplinary team delivering a computer-vision garment defect detection system achieving 87% IoU, forecast to improve client's overall productivity by 2%;

- Integrated a real-time robotic QA system improving labelling defect detection by 30% over trained human inspectors, saving the client's ~£200k/year;

- Designed a novel turn-taking detection algorithm improving conversation completion rates by 13%; subsequently adopted by Honda Research Institute Japan's LLM division;

- Lead development of cloud data platform, for oil rig automated video-based inspections, leveraging Azure Data Lake and Batch Service, Terraform-automated infrastructure, and a WebAssembly Blazor frontend;


---
## Education

**MEng Software Engineering · Heriot-Watt University, Edinburgh**
Sep 2018 – Jul 2023 · Awarded Distinction

---
## Projects & Open Source

**Football Analytics Engine** · (in development)
Proprietary Haskell application (fully Nixified) that ingests in-match football event streams and derives novel performance metrics not available in current commercial data products. Personal entrepreneurial project targeting eventual commercial launch.

**P2PRC** · Open-Source Contributor
Developed and maintained the Haskell language layer on top of the P2PRC networking library, enabling industrial adoption by external organisations, such as <TODO: add link to Kompanion>. Managed builds, development environments, and deployments with Nix; Leading Haskell API development; Contributor to software architecture and system integration planning.

**Docker Development Framework** · Past experience's internal tooling
Spontaneously developed a lightweight Bash framework that standardised Docker development and deployment workflows across multiple industrial projects, during job experience National Robotarium, collectively worth +500k pounds in projects' revenue; included Nvidia Docker runtime integration and X11 desktop application support, essential for robotic's development.

**Ouros - Rust Hardware Simulator** · Rust Open-source research project
<TODO: add link to this emulator https://github.com/bathtub-01/ouros-simulator >
Informal collaboration with a research group focused in Haskell based dataflow hardware description. This emulator takes a takes a MicroHS combinator-based AST program representation to be compiled into an hardware Haskell runtime representation. Currently being part of preparing a open-source public release by taking responsibility for upgrading cargo and nix development/build environment, as well as optimising a
<TODO: responsible for hardening static analysis of rust project>
<TODO: responsible for adding rust profiling, will be optimising a (performance) naive implmentation>


---
## Languages

**English** – Full professional proficiency · **Portuguese** – Native · **Spanish** – Professional proficiency
