# Jose Fernandes
**Rust Systems Software Engineer**

+44 7759 669644 · jf94.uk@gmail.com · linkedin.com/in/xecarlox94 · github.com/xecarlox94 · UK (Indefinite Leave to Remain)

---

## Summary

Rust systems software engineer with 1–2 years of professional Rust experience designing and building concurrent, safety-critical distributed systems from scratch. Specialist in actor-model architecture, async/await concurrency, and industrial communication protocols (Modbus, protobuf, HTTP/REST) for critical infrastructure and energy systems. Proven track record leading multi-disciplinary engineering teams to deliver measurable production outcomes. Additional experience in computer vision, real-time robotics, and cloud data engineering.

---

## Skills

**Languages:** Rust · Haskell · OCaml · C · C++ · Python · Java · Scala · SQL · Terraform · Bash

**Rust ecosystem:** tokio · async/await · actor model · sqlx · channels · select! · cancellation tokens · task trackers · property-based testing · WebAssembly (WASM)

**Systems & protocols:** Modbus · Protocol Buffers (protobuf) · HTTP · REST · Linux · Nix · Docker · RaspberryPi GPIO · Arduino · Nvidia Jetson

**Databases:** PostgreSQL · Azure SQL · NoSQL

**Cloud:** Azure (Synapse · Functions · Batch · Data Lake · Event Hub) · AWS (S3 · EC2) · GCP (DataProc · BigQuery)

**Domains:** critical infrastructure · energy systems · distributed systems · concurrent systems · memory safety · low-latency · fault tolerance · real-time systems · computer vision · smart contracts

**Also:** PyTorch · OpenCV · smart contract integration (Rust)

---

## Experience

### Rust Software Engineer · Enoda Ltd · UK
**Mar 2025 – Present**

Sole architect and lead Rust engineer on the ENSEMBLE™ aggregator platform — a market- and hardware-agnostic system for real-time bidding of grid capacity across TSO regions and metering device vendors, supporting the transition to renewable energy critical infrastructure.

- Designed the system architecture and implemented an actor-model application in Rust on tokio from scratch, comprising: a Core actor for bid aggregation and financial calculations with PostgreSQL persistence via sqlx; a Device actor for metering device communication over Modbus and protobuf; and a Wire actor for TSO grid command interfaces over HTTP/REST.
- Architected for vendor and market agnosticism: designed an SDK abstraction layer supporting multiple hardware vendors and a pluggable market adapter pattern for different regional energy market APIs.
- Engineered concurrency and resource safety using tokio primitives — channels, select!, cancellation tokens, and task trackers — with graceful actor supervision and fault-tolerant shutdown.
- Built comprehensive test coverage across unit, integration, and property-based testing disciplines to ensure correctness ahead of production scale-up.
- Currently in integration testing with a European TSO; architecture designed to scale from pilot deployments to hundreds of thousands of devices.

---

### Software Engineer · National Robotarium · Edinburgh, UK
**November 2022 – March 2025**

Delivered production-grade perception, data, and control systems across a portfolio of industrial AI and robotics R&D projects for major clients, progressing to project lead. Work spanned computer vision, real-time robotics, conversational AI, and cloud data engineering.

- Led a multi-disciplinary team delivering a computer-vision garment defect detection system achieving 87% IoU, forecast to improve client productivity by 2% at the next TRL stage.
- Integrated a real-time robotic QA system improving labelling defect detection by 30% over trained human inspectors, saving the client ~£200k/year.
- Designed a novel turn-taking detection algorithm improving conversation completion rates by 13%; subsequently adopted by Honda Research Institute Japan's LLM division.
- Productionised perception pipeline notebooks into reliable async/real-time modules on Nvidia Jetson hardware with robotic control, PyTorch, and OpenCV.
- Delivered a full-stack cloud data platform with parallel ingestion pipelines on Azure Data Lake and Batch Service, Terraform-automated infrastructure, and a Blazor/C# ASP.NET frontend.

---

## Education

**MEng Software Engineering · Heriot-Watt University, Edinburgh**
September 2018 – July 2023 · Distinction

Relevant areas: concurrent systems, distributed computing, formal methods, software architecture

---

## Projects & Open Source

**Football Analytics Engine** *(in development)*
Proprietary Haskell application (fully Nixified) ingesting live in-match football event streams to derive novel performance metrics not available in commercial data products. Personal project targeting commercial launch.

**P2PRC** · Open-Source Contributor
Developed and maintained the Haskell language layer on top of the P2PRC networking library, enabling industrial adoption by external organisations. Managed builds, development environments, and deployments with Nix.

**Docker Development Framework** · Internal Tooling
Built a Bash framework standardising Docker development and deployment workflows across multiple industrial projects collectively worth hundreds of thousands of pounds; included Nvidia Docker runtime integration and X11 desktop application support.

---

## Languages

**English** – Full professional proficiency · **Portuguese** – Native · **Spanish** – Professional proficiency

