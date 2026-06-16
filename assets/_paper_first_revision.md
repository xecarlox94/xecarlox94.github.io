# Jose Fernandes
**Rust Systems Software Engineer**

+44 7759 669644 · jf94.uk@gmail.com · linkedin.com/in/xecarlox94 · github.com/xecarlox94

---

## Professional Summary

Systems software engineer specialising in software architecture of real-time, safety-critical Rust applications for industrial and energy infrastructure. Experienced in designing actor-model concurrent systems with Rust, integrating industrial protocols, and building reliable software for environments where correctness and resilience matter. Track record of leading multi-disciplinary engineering teams to deliver measurable outcomes for major industrial clients. Particularly interested in critical systems, verified software, and the intersection of robust engineering with complex operational domains.

- v2
Rust systems software engineer with 2 years of professional Rust experience designing and building concurrent, safety-critical distributed systems. Specialist in actor-model architecture, async/await concurrency, and industrial communication protocols (Protobuf, HTTP/REST) for critical infrastructure and energy systems. Proven track record leading multi-disciplinary engineering teams to deliver measurable production outcomes. Additional experience in computer vision, real-time robotics, and cloud data engineering.


---


## Skills

**Rust:** tokio (channels, select!, cancellation tokens, task trackers), sqlx, actor model, async/await, property testing

**Databases:** PostgreSQL (sqlx), Azure SQL, NoSQL

**Languages:** Rust, Haskell, OCaml, C/C++, Nix, Java/Scala, Python, Terraform

**Systems:** Linux, NixOS, Docker, RaspberryPi GPIO, Arduino, Nvidia Jetson, Protocol Buffers (protobuf), HTTP/REST, WebSockets

**Cloud:** Azure (Synapse, Functions, Batch, Data Lake, Event Hub), AWS (S3, EC2), GCP (DataProc, BigQuery)

**Also:** Rust smart contract integration (sui and cosmos sdks); PyTorch; OpenCV


---

## Experience

### Rust Software Engineer · Enoda Ltd · UK
**Mar 2025 – Present**

Part of the ENSEMBLE™ division building the aggregator: a market- and hardware-agnostic
platform that bids grid capacity in real time across TSO regions and metering device vendors,
supporting the transition to renewable energy infrastructure.

- Designed and implemented an actor-model application in Rust on tokio, comprising: a Core
  actor for bid aggregation and financial calculations with PostgreSQL persistence via sqlx;
  a Device actor for metering device communication over Modbus and protobuf; and a WIRE actor
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


- v2

Architected and lead Rust engineer on the ENSEMBLE™ aggregator platform — a market- and hardware-agnostic system for real-time bidding of grid capacity across TSO regions and metering device vendors, supporting the transition to renewable energy critical infrastructure.

- Designed the system architecture and implemented an actor-model application in Rust on tokio from scratch, comprising: a Core actor for bid aggregation and financial calculations with PostgreSQL persistence via sqlx; a Device actor for metering device communication over Modbus and protobuf; and a Wire actor for TSO grid command interfaces over HTTP/REST.
- Architected for vendor and market agnosticism: designed an SDK abstraction layer supporting multiple hardware vendors and a pluggable market adapter pattern for different regional energy market APIs.
- Engineered concurrency and resource safety using tokio primitives — channels, select!, cancellation tokens, and task trackers — with graceful actor supervision and fault-tolerant shutdown.
- Built comprehensive test coverage across unit, integration, and property-based testing disciplines to ensure correctness ahead of production scale-up.
- Currently in integration testing with a European TSO; architecture designed to scale from pilot deployments to hundreds of thousands of devices.

---
### Software Engineer · National Robotarium · Edinburgh, UK
**Nov 2022 – Mar 2025**

Worked across a portfolio of industrial AI and robotics R&D projects for major UK clients, progressing to project lead. Delivered production ready perception, data, and control systems from client brief through to on-site deployment, spanning computer vision, real-time robotics, conversational AI, and cloud data engineering.

- Led a multi-disciplinary team delivering a computer-vision garment defect detection system
  achieving 87% IoU, forecast to improve client productivity by 2% at the next TRL stage.

- Integrated a real-time robotic QA system improving labelling defect detection by 30% over
  trained human inspectors, saving the client ~£200k/year.

- Designed a novel turn-taking detection algorithm improving conversation completion rates by
  13%; subsequently adopted by Honda Research Institute's LLM division.

- Productionised perception pipeline notebooks into reliable async/real-time modules on Nvidia Jetson hardware with robotic control, PyTorch, and OpenCV.

- Delivered a full-stack cloud data platform with parallel ingestion pipelines on Azure Data Lake and Batch Service, Terraform-automated infrastructure, and a Blazor/C# ASP.NET frontend.

- v2

- Led a multi-disciplinary team delivering a computer-vision garment defect detection system achieving 87% IoU, forecast to improve client productivity by 2% at the next TRL stage.

- Integrated a real-time robotic QA system improving labelling defect detection by 30% over trained human inspectors, saving the client ~£200k/year.

- Designed a novel turn-taking detection algorithm improving conversation completion rates by 13%; subsequently adopted by Honda Research Institute Japan's LLM division.

- Productionised perception pipeline notebooks into reliable async/real-time modules on Nvidia Jetson hardware with robotic control, PyTorch, and OpenCV.

- Delivered a full-stack cloud data platform with parallel ingestion pipelines on Azure Data Lake and Batch Service, Terraform-automated infrastructure, and a Blazor/C# ASP.NET frontend.



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
