# Jose Fernandes
**Rust Systems Software Engineer**

+44 7759 669644 · jf94.uk@gmail.com · linkedin.com/in/xecarlox94 · github.com/xecarlox94

---

## Professional Summary

Systems software engineer specialising in real-time, safety-critical Rust applications for
industrial and energy infrastructure. Experienced in designing actor-model concurrent systems
with Rust, integrating industrial protocols, and building reliable software for environments
where correctness and resilience matter. Track record of leading multi-disciplinary engineering
teams to deliver measurable outcomes for major industrial clients. Particularly interested in
critical systems, verified software, and the intersection of robust engineering with complex
operational domains.

*Immigration status: Indefinite Leave to Remain (UK)*

---

## Technical Skills

**Rust:** tokio (channels, select!, cancellation tokens, task trackers), sqlx, actor model,
async/await, property testing

**Protocols:** Modbus, Protocol Buffers (protobuf), HTTP/REST

**Databases:** PostgreSQL (sqlx), Azure SQL, NoSQL

**Languages:** Rust, Haskell, OCaml, C/C++, Java/Scala, Python, SQL, Terraform

**Systems:** Linux, Nix, RaspberryPi GPIO, Arduino, Nvidia Jetson, Docker

**Cloud:** Azure (Synapse, Functions, Batch, Data Lake, Event Hub), AWS (S3, EC2),
GCP (DataProc, BigQuery)

**Also:** Smart contract integration (Rust); PyTorch; OpenCV

---

## Experience

### Rust Software Engineer · Enoda Ltd · UK
**May 2024 – Present**

Part of the ENSEMBLE™ division building the aggregator: a market- and hardware-agnostic
platform that bids grid capacity in real time across TSO regions and metering device vendors,
supporting the transition to renewable energy infrastructure.

- Designed and implemented an actor-model application in Rust on tokio, comprising: a Core
  actor for bid aggregation and financial calculations with PostgreSQL persistence via sqlx;
  a Device actor for metering device communication over Modbus and protobuf; and a Wire actor
  for TSO grid command interfaces over HTTP/REST.

- Architected the system for vendor and market agnosticism: developed an SDK abstraction layer
  supporting multiple hardware vendors, and a pluggable market adapter pattern for different
  regional energy market APIs.

- Engineered robust concurrency and resource safety using tokio primitives — channels, select!,
  cancellation tokens, and task trackers — with graceful supervision of all actors and resources.

- Built extensive test coverage across unit, integration, and property-based testing disciplines
  to ensure system correctness ahead of production scale-up.

- Currently in integration testing with a European TSO; platform architecture designed to scale
  from initial pilot deployments to hundreds of thousands of devices.

---

### Software Engineer · National Robotarium · Edinburgh, UK
**Nov 2022 – Mar 2025**

Worked across a portfolio of industrial AI and robotics R&D projects for major clients,
progressing to project lead. Delivered production-grade perception, data, and control systems
from client brief through to on-site deployment, spanning computer vision, real-time robotics,
conversational AI, and cloud data engineering.

- Led a multi-disciplinary team delivering a computer-vision garment defect detection system
  achieving 87% IoU, forecast to improve client productivity by 2% at the next TRL stage.

- Integrated a real-time robotic QA system improving labelling defect detection by 30% over
  trained human inspectors, saving the client ~£200k/year.

- Designed a novel turn-taking detection algorithm improving conversation completion rates by
  13%; subsequently adopted by Honda Research Institute Japan's LLM division.

- Productionised perception pipeline notebooks into reliable async/real-time modules on
  Nvidia Jetson hardware with robotic control, PyTorch, and OpenCV.

- Delivered a full-stack cloud data platform with parallel ingestion pipelines on Azure Data
  Lake and Batch Service, Terraform-automated infrastructure, and a Blazor/C# ASP.NET frontend.

---

## Education

**MEng Software Engineering · Heriot-Watt University, Edinburgh**
Sep 2018 – Jul 2023 · Awarded Distinction

---

## Projects & Open Source

**Football Analytics Engine** *(private, in development)*
Designing and building a proprietary Haskell application (fully Nixified) that ingests live
in-match football event streams and derives novel performance metrics not available in commercial
data products. Personal entrepreneurial project targeting eventual commercial launch.

**P2PRC** · Open-Source Contributor
Developed and maintained the Haskell language layer on top of the P2PRC networking library,
enabling industrial adoption by external organisations. Managed builds, development environments,
and deployments with Nix; contributed to software architecture and system integration planning.

**Docker Development Framework** · Internal Tooling
Built a lightweight Bash framework that standardised Docker development and deployment workflows
across multiple industrial projects collectively worth hundreds of thousands of pounds; included
Nvidia Docker runtime integration and X11 desktop application support.

---

## Languages

**English** – Full professional proficiency · **Portuguese** – Native · **Spanish** – Professional proficiency
