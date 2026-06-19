# Jose Fernandes
**Rust Systems Software Engineer**

+44 7759 669644 
jf94.uk@gmail.com 
linkedin.com/in/xecarlox94 
github.com/xecarlox94

---
## Professional Summary


/* LLM AGENT EDITING AREA
LLM AGENT instruction: there are two versions of a cv header (I want to write up the final one, propose one based on the context of my entire cv) I also have another comment that I would like to add

// version 1 
Systems software engineer specialising in software engineering and architecture of real-time, safety-critical Rust applications for industrial and critical infrastructure. Experienced in designing actor-model concurrent systems with Rust, integrating industrial protocols, and building reliable software for environments where correctness and resilience matter. Track record of leading multi-disciplinary engineering and research teams to deliver measurable outcomes for major industrial clients. Particularly interested in critical systems, verified software, and the intersection of robust engineering with complex operational domains.

// version 2 

Rust systems software engineer with 1-2 years of professional Rust experience designing and building concurrent, safety-critical distributed systems. Specialist in actor-model architecture, async/await concurrency, and industrial communication protocols (Protobuf, HTTP/REST) for critical infrastructure and energy systems. Proven track record leading multi-disciplinary engineering and research teams to deliver measurable production outcomes. Additional experience in computer vision, real-time robotics, and cloud data engineering.


// something I would like to add to whichever alternative

Business-driven yet curious for cutting edge programming language and software verification research to offer competitive advantage to employer/client

*/



---
## Skills

**Rust:** async/await, tokio (channels, select!, cancellation tokens, task trackers), sqlx, actor model, software testing

**Languages:** Rust, Haskell, Python, Ocaml/Rocq, Java/Scala, Nix, C/C++, C#/F#, Terraform

**Systems:** Linux, NixOS, Docker, RaspberryPi GPIO, Nvidia Jetson, WebAssembly, HTTP/REST, WebSockets, Protobuf, Open Telemetry (Grafana)

**Databases:** PostgreSQL (sqlx), Azure SQL, NoSQL

**Cloud:** Azure (Synapse, Functions, Data Lake, Event Hub), AWS (S3, EC2), GCP (DataProc, BigQuery)

**Also:** Type-driven development; Rust smart contract integration (sui and cosmos sdks);

---
## Experience

### Rust Software Engineer - Enoda Ltd - UK
**Mar 2025 – Present**



/* LLM AGENT EDITING AREA
LLM AGENT instruction: I want to write up a concise job experience header for this job experience. one job experience header section and 2 smaller bullet points that expand on the aspects reffered in extra comments. Alternatively you can recommend a different structure for this area

// (SECTION: JOB experience header) 

- Architected the system for vendor and market agnosticism: developed an SDK supporting multiple hardware vendors, and a pluggable market adapter pattern for different regional energy market APIs.

Architected and lead Rust engineer on a new electrical aggregator platform — a market- and hardware-agnostic system for real-time bidding of grid capacity across TSO (Transmission Service Operator) regions and metering device vendors, supporting the transition to renewable energy critical infrastructure.

// Comment: Influenced the technical and business aspects of the application aligning the development of a new platform with global ambitions

// Comment: this an sdk based framework to consistently and quickly bootstrap electrical aggregators>
// Comment: new platform, built its own primitives


// (SECTION: Architecture principles 1)

// Comment: Functorial/Monadic layering to stricly segment resposibilities across the framework layers
// Comment: deliberate usage of actor architecture to unify business functional requirements (and their updates)
// Comment: explain that the actor-model architecture is matching the functional requirements allowing effective tracking of current (and future changes) to specification


// (SECTION: Architecture principles 2) 

- Designed and implemented an actor-model application in Rust on tokio and postgres sqlx
// Comment: embedding business logic into type system to track functional requirements and their updates
// Comment:typestate pattern to enforce algebraic-driven parsing and business-level validation


*/



- Engineered robust IO-bounded concurrency model, leveraging tokio primitives, with fault-tolerant supervision of running instances and their internal resources;

- Installed a monitoring and alerting layer, leveraging open-telemetry and grafana, to trace application and business operations, and handle to certain kinds of runtime errors;

- Setup comprehensive unit testing coverage across framework's components and functional requirements driven integration testing and aligned with the actor-model;

- Currently in integration testing with an European TSO;

---
### Software Engineer - National Robotarium - Edinburgh, UK
**Nov 2022 – Mar 2025**

Worked across a portfolio of industrial production-driven AI and Robotics R&D projects for major UK industrial clients, progressing to project lead engineer;

- Led a multi-disciplinary team delivering a computer-vision garment defect detection system achieving 87% IoU, forecast to improve client's overall productivity by 2%;

- Integrated a real-time robotic QA system improving labelling defect detection by 30% over trained human inspectors, saving the client's ~£200k/year;

- Designed a novel turn-taking detection algorithm improving conversation completion rates by 13%; subsequently adopted by Honda Research Institute Japan's LLM division;

- Lead development of cloud data platform, for oil rig automated video-based inspections, leveraging Azure Data Lake and Batch Service, Terraform-automated infrastructure, and a WebAssembly Blazor frontend;


---
## Education

**MEng Software Engineering - Heriot-Watt University, Edinburgh**
Sep 2018 – Jul 2023: Awarded Distinction

---
## Projects & Open Source

**Football Analytics Engine** - (in development)
Proprietary Haskell application (fully Nixified) that ingests in-match football event streams and derives novel performance metrics not available in current commercial data products. Personal entrepreneurial project targeting eventual commercial launch.

**P2PRC** - Open-Source Contributor
Developed and maintained the Haskell language layer on top of the P2PRC networking library, enabling industrial adoption by external organisations, such as <TODO: add link to Kompanion>. Managed builds, development environments, and deployments with Nix; Leading Haskell API development; Contributor to software architecture and system integration planning.

**Docker Development & Deployment environment** - Past experience's internal tooling
Spontaneously developed a lightweight Bash framework that standardised Docker development and deployment workflows across multiple contracted projects (worth +500k pounds in revenue), during job experience National Robotarium; included Nvidia Docker runtime integration and X11 desktop application support, essential for robotic's development.


---
## Languages

**English** – Full professional proficiency 
**Portuguese** – Native 
**Spanish** – Professional proficiency

