# Jose Fernandes
**Rust Systems Software Engineer**

+44 7759 669644 
jf94.uk@gmail.com 
linkedin.com/in/xecarlox94 
github.com/xecarlox94

---
## Professional Summary


/* LLM AGENT EDITING AREA
LLM AGENT instruction: I am finishing this small introdution. give a review of this editing area, according to the profile I am targetting. should I change anything?


Systems software engineer, with 1-2 years of rust development experience, designing and building concurrent, safety-critical distributed systems. Specialist in actor-model architecture, async/await concurrency, and industrial communication protocols (HTTP/REST, Protobuf) for critical infrastructure and energy systems. Proven track record leading multi-disciplinary engineering teams to deliver measurable production outcomes. Additional experience in computer vision, real-time robotics, and cloud data engineering. Business-driven, with a continued interest in cutting-edge programming language and software verification latest research as a source of industrial competitive advantage.


*/



---
## Skills

**Rust:** async/await, tokio (channels, select!, cancellation tokens, task trackers), sqlx, actor model, software testing

**Languages:** Rust, Haskell, Ocaml/Rocq, Python, Java/Scala, C/C++, Nix, Terraform

**Systems:** Linux, NixOS, Docker, RaspberryPi GPIO, Nvidia Jetson, WebAssembly, HTTP/REST, WebSockets, Protobuf, Open Telemetry (Grafana)

**Databases:** PostgreSQL (sqlx), Azure SQL, NoSQL

**Cloud:** Azure (Synapse, Functions, Data Lake, Event Hub), AWS (S3, EC2), GCP (DataProc, BigQuery)

**Also:** Type-driven development; Rust smart-contract integration (sui and cosmos sdks);

---
## Experience

### Rust Software Engineer - Enoda Ltd - UK
**Mar 2025 – Present**


/* LLM AGENT EDITING AREA
LLM AGENT instruction: I am still doing some reworking. give a review of this editing area, according to the profile I am targetting. should I change anything? should I add anything?


// (SECTION: JOB experience header) 


Co-engineered and co-architected, a new electrical aggregator platform — a market- and hardware-agnostic, SDK-based framework for real-time bidding of grid capacity across TSO (Transmission Service Operator) regions and metering device vendors, supporting the standardisation of energy market's financial operations. Influenced both the technical architecture and business direction of the platform in line with the company's goals and vision, building a new set of primitives from the ground up to consistently and quickly bootstrap electrical aggregators.

++
Co-engineered and co-architected, as part of a two-person Rust team, a new electrical aggregator platform — a market- and hardware-agnostic SDK built on a highly composable, pluggable actor-model architecture for real-time bidding of grid capacity across TSO (Transmission Service Operator) regions and metering device vendors, supporting the transition to renewable energy critical infrastructure. Influenced both the technical architecture and business direction of the platform in line with the company's global ambitions, building a new set of primitives from the ground up to consistently and quickly bootstrap electrical aggregators.
++




// (SECTION: Architecture principles 1)

// Comment: deliberate usage of actor architecture TO UNIFY business functional requirements (and their updates)
// actor model was also chosen to allow high modularity and composability (needed to support be generic across markets and devices); NEED TO SELL THIS MORE!!

- Designed and implemented an actor-model application in Rust on tokio and PostgreSQL (sqlx), using functorial/monadic layering to strictly segment responsibilities across framework layers; the actor-model structure was chosen to mirror business functional requirements directly, enabling effective tracking of current and future specification changes.

++
- Designed and implemented an actor-model application in Rust on tokio and PostgreSQL (sqlx), using functorial/monadic layering to strictly segment responsibilities across framework layers; the actor-model structure was chosen to unify business functional requirements directly with implementation and technical spec documentation, enabling effective tracking of current and future specification changes.
++


// (SECTION: Architecture principles 2) 

- Embedded business logic into the type system to statically track functional requirements and their updates, applying the typestate pattern to enforce algebraic-driven parsing and business-level validation.

// COMMENT: maybe add code and technical documentation? maybe add it to integration testing section

++
- Leveraged the actor model's modularity and composability to support genericity across markets and hardware vendors, allowing new aggregator deployments to be assembled from existing, pluggable components rather than rebuilt from scratch.
++

++
- Embedded business logic into the type system to statically track functional requirements and their updates, applying the typestate pattern to enforce algebraic-driven parsing and business-level validation at compile time.
++

*/


- Engineered robust IO-bounded concurrency model, leveraging tokio primitives, with fault-tolerant supervision of running instances and their internal resources;

- Setup comprehensive unit testing coverage across framework's components and functional requirements driven integration testing and aligned with the actor-model;

- Installed a monitoring and alerting layer, leveraging open-telemetry and grafana, to trace application and business operations, and handle to certain kinds of runtime errors;

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
Proprietary Haskell application (nixified infrastructure) that ingests in-match football event streams and derives novel performance metrics not available in current commercial data products. Personal entrepreneurial project targeting eventual commercial launch.

**P2PRC** - Open-Source Contributor
Developed and maintained the Haskell language layer on top of the P2PRC networking library, enabling industrial adoption by external organisations, such as <TODO: add link to Kompanion>. Managed builds, development environments, and deployments with Nix; Leading Haskell API development; Contributor to software architecture and system integration planning.

**Docker Development & Deployment environment** - Past experience's internal tooling
Spontaneously developed a lightweight Bash framework that standardised Docker development and deployment workflows across multiple contracted projects (worth +500k pounds in revenue), during job experience National Robotarium; included Nvidia Docker runtime integration and X11 desktop application support, essential for robotic's development.


---
## Languages

**English** – Full professional proficiency 
**Portuguese** – Native 
**Spanish** – Professional proficiency

