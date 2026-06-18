# Jose Fernandes
**Rust Systems Software Engineer**

+44 7759 669644 · jf94.uk@gmail.com · linkedin.com/in/xecarlox94 · github.com/xecarlox94

---
## Professional Summary

Systems software engineer specialising in software engineering and architecture of real-time, safety-critical Rust applications for industrial and critical infrastructure. Experienced in designing actor-model concurrent systems with Rust, integrating industrial protocols, and building reliable software for environments where correctness and resilience matter. Track record of leading multi-disciplinary engineering and research teams to deliver measurable outcomes for major industrial clients. Particularly interested in critical systems, verified software, and the intersection of robust engineering with complex operational domains.

- v2

Rust systems software engineer with 1-2 years of professional Rust experience designing and building concurrent, safety-critical distributed systems. Specialist in actor-model architecture, async/await concurrency, and industrial communication protocols (Protobuf, HTTP/REST) for critical infrastructure and energy systems. Proven track record leading multi-disciplinary engineering and research teams to deliver measurable production outcomes. Additional experience in computer vision, real-time robotics, and cloud data engineering.


---
## Skills

**Rust:** async/await, tokio (channels, select!, cancellation tokens, task trackers), sqlx, actor model, property testing
<TODO: add testing>

**Databases:** PostgreSQL, Azure SQL, NoSQL

**Languages:** Rust, Haskell, OCaml, Python, Nix, Java/Scala, C/C++, Terraform

**Systems:** Linux, NixOS, Docker, RaspberryPi GPIO, Nvidia Jetson, WebAssembly, Protocol Buffers (protobuf), Open Telemetry, HTTP/REST, WebSockets

**Cloud:** Azure (Synapse, Functions, Batch, Data Lake, Event Hub), AWS (S3, EC2), GCP (DataProc, BigQuery)

**Also:** Rust smart contract integration (sui and cosmos sdks); PyTorch; OpenCV

---
## Experience

### Rust Software Engineer · Enoda Ltd · UK
**Mar 2025 – Present**

Part of the ENSEMBLE division building the aggregator: a market- and hardware-agnostic platform that bids grid capacity in real time across TSO regions and metering device vendors, supporting the transition to renewable energy infrastructure.

Architected and lead Rust engineer on the ENSEMBLE aggregator platform — a market- and hardware-agnostic system for real-time bidding of grid capacity across TSO regions and metering device vendors, supporting the transition to renewable energy critical infrastructure.

Influenced the technical and business aspects of the application aligning the development of a new platform with global ambitions

new platform, built its own primitives


- Designed and implemented an actor-model application in Rust on tokio, comprising: a Core actor for bid aggregation and financial calculations with PostgreSQL persistence via sqlx; a Device actor for metering device communication over protobuf; and a WIRE actor for TSO grid command interfaces over HTTP/REST.
<TODO: explain that the actor-model architecture is matching the functional requirements allowing effective tracking of current (and future changes) to specification>


- Architected the system for vendor and market agnosticism: developed an SDK abstraction layer supporting multiple hardware vendors, and a pluggable market adapter pattern for different regional energy market APIs and different kinds of devices.


<TODO: need to finish open-telemetry>
- installed a telemetry and observability layer, using open-telemetry, which traces every application and business logic operation and handles error handling
alerting 
monitoring

- Engineered robust IO-bounded concurrency and resource safety, leveraging tokio primitives, with fault-tolerant supervision of running instances and their internal resources.

- Built comprehensive test coverage across unit, integration, and property-based testing disciplines to ensure correctness ahead of production scale-up.

- Currently in integration testing with an European TSO;

---
### Software Engineer (Project Lead) · National Robotarium · Edinburgh, UK
**Nov 2022 – Mar 2025**

Worked across a portfolio of industrial production-driven AI and Robotics R&D projects for major UK industrial clients, progressing to project lead engineer.

- Led a multi-disciplinary team delivering a computer-vision garment defect detection system achieving 87% IoU, forecast to improve client's overall productivity by 2%.

- Integrated a real-time robotic QA system improving labelling defect detection by 30% over trained human inspectors, saving the client's ~£200k/year.

- Designed a novel turn-taking detection algorithm improving conversation completion rates by 13%; subsequently adopted by Honda Research Institute Japan's LLM division.

- Lead development of cloud data platform, for oil rig automated video-based inspections, leveraging Azure Data Lake and Batch Service, Terraform-automated infrastructure, and a WebAssembly Blazor frontend.


---
## Education

**MEng Software Engineering · Heriot-Watt University, Edinburgh**
Sep 2018 – Jul 2023 · Awarded Distinction

---
## Projects & Open Source

**Football Analytics Engine** · (in development)
Proprietary Haskell application (fully Nixified) that ingests in-match football event streams and derives novel performance metrics not available in current commercial data products. Personal entrepreneurial project targeting eventual commercial launch.

**P2PRC** · Open-Source Contributor
Developed and maintained the Haskell language layer on top of the P2PRC networking library, enabling industrial adoption by external organisations <TODO: add link to Kompanion>. Managed builds, development environments, and deployments with Nix; contributed to software architecture and system integration planning.

**Docker Development Framework** · Internal Tooling
Built a lightweight Bash framework that standardised Docker development and deployment workflows across multiple industrial projects, at National Robotarium, collectively worth hundreds of thousands of pounds; included Nvidia Docker runtime integration and X11 desktop application support.

---
## Languages

**English** – Full professional proficiency · **Portuguese** – Native · **Spanish** – Professional proficiency
