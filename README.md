# ETrace - Electron Tracer
![grafik](https://github.com/wp-xyz/ETrace/assets/30792460/04a6d097-3d41-467a-aa41-2d77e5c86cc9)

ETrace is a simulation project developed using Free Pascal and Lazarus, designed to trace electron trajectories and simulate Auger electron emission from various sample geometries.

## Project Overview

This section provides a high-level description of the ETrace project, its purpose, and its main components.

ETrace is a simulation software built with Free Pascal and Lazarus. Its primary objective is to model and visualize the interaction of primary electrons with various material samples, specifically focusing on the generation and emission of Auger electrons. The simulation employs Monte Carlo methods to trace the trajectories of electrons within the material, accounting for scattering and energy loss processes.

The project provides a graphical user interface (GUI) that allows users to:
- Configure the parameters of the electron gun (energy, beam diameter, focus).
- Define the characteristics and geometry of the sample, including substrate and layer materials, layer thickness, and topography (contact hole, stripe, step).
- Specify the type and acceptance criteria of the electron analyzer.
- Run simulations based on the defined parameters.
- Visualize the results, such as the emission points of Auger electrons and selected electron trajectories.
- View a summary of the simulation results, including total and spatially resolved Auger intensities.

The core simulation logic is implemented in Pascal units, while the GUI is built using the Lazarus Component Library (LCL).

## Simulation Model

The heart of ETrace lies in its simulation engine, primarily implemented in `et_objects.pas` and `et_sim.pas`. It employs a Monte Carlo approach to simulate the complex interactions of electrons with the sample material.

Key aspects of the simulation model include:

1.  **Electron Source**:
    - Generates primary electrons with a specified primary energy (keV) and beam diameter (µm).
    - Electrons are focused towards a defined focus point on the sample surface, specified in the sample's (X,Y,Z) coordinate system.
    - The beam can be tilted relative to the sample normal (positive Z-axis) using the `TiltAngle` (degrees). The azimuthal angle of the primary beam is fixed to 0 degrees (in the XZ plane).
    - The `GenerateElectron` procedure creates individual primary electrons with their initial position and direction, incorporating a normal distribution spread around the focused point.

2.  **Electron-Material Interaction**:
    - The `TMaterial` class encapsulates the physical properties of different materials (e.g., Si, SiO2, Au) relevant to electron interactions.
    - It stores fundamental parameters like atomic number Z,  atomic/molecular mass A, mass density (g/cm³),  core level energy (keV), and Auger electron energy` (eV). .
    - It provides methods based on established physics models to calculate:
        - the ionization cross section for exciting the core level at electron energy E, based on the Gryzinski model (Eq. 11 in Ze-jun et al. SIA, 10, 253 (1987)).
        - the average distance (in µm) an Auger electron with energy E can travel within the material before losing energy or being absorbed, based on the Seah & Dench model.
        - the rate at which an electron loses energy (dE/dS) as it travels through the material, for energy E (keV), using a modified Bethe equation (See Eli Napchan, p9). The result is in keV/µm.
        - total elastic scattering cross-section and the screening factor for energy E (keV), based on the Rutherford scattering model with a screening correction (See Eli Napchan, p10). These parameters are used to determine scattering angles and step lengths.

3.  **Sample Geometry and Interaction**:
    - The `TSample` is an abstract base class defining the common interface and shared logic for sample objects. It is initialized with substrate and layer materials, the primary electron energy, and the layer thickness.
    - It manages the `Substrate` and `Layer` materials and the layer-substrate interface position (negative Z relative to the top surface). It changes the active material based on the electron's current Z-coordinate.
    - The core interaction logic, specific to the sample's shape, is handled by methods that must be implemented by derived classes:
        - `OnSurface(Point: TVector3)`: Returns `True` if the given `Point` is on the sample surface, considering the geometrical tolerance `FloatEps`.
        - `Outside(Point: TVector3)`: Returns `True` if the given `Point` is outside the sample volume.
        - `Intersection(Ray: TRay, var Point: TVector3, FromOutside: Boolean)`: Calculates the intersection point (`Point`) of an electron `Ray` with the sample surface. It returns `True` if an intersection exists in the positive ray direction. The `FromOutside` parameter indicates whether the ray is approaching the surface from outside the sample volume.
        - `SurfNormal(Point: TVector3, var Normal: TVector3)`: Calculates the surface normal vector (`Normal`) at a given `Point` on the surface.
    - **Derived Classes**: `TContactHole`, `TStripe`, and `TStep` provide concrete implementations of the sample interface for specific geometries, defining their shapes and implementing the intersection and surface normal logic based on planes and cylinders. 
    - Simulated Auger electron emission occurs when a primary or scattered electron arrives at a surface point with sufficient energy (`E >= Material.CoreLevelEnergy`). The emission direction is randomized isotropically into the upper hemisphere defined by the surface normal. The contribution of the emitted Auger electron to the overall intensity in the detector depends on the material's ionization cross-section, escape depth, emission angle relative to the surface normal, and a normalization factor. The model also takes care of Auger electrons generated by backscattered electrons.
    - A Monte Carlo step implements electron elastic scattering. Based on the current electron energy and the material properties, it calculates a new direction and energy after traveling a simulated step distance. The step length is determined probabilistically based on the total scattering cross-section. Energy loss is calculated based on the step length and stopping power. The scattering angle distribution is based on the screened Rutherford model.

4.  **Electron Trajectory Calculation (`TSimulation.CalcTrajectory`)**:
    - This procedure traces the path of a single electron (primary or scattered) step-by-step within the sample using the `FSample.Scatter` method.
    - At each step, it checks if the electron has crossed the layer-substrate interface and updates the active material accordingly.
    - It checks for intersections with the sample boundary and records the exit point if the electron leaves the sample.
    - If the electron is on the surface and has sufficient energy (`E >= Material.CoreLevelEnergy`), it triggers the emission of an Auger electron.
    - The trajectory points (position and energy) can be stored in a trajectory array.
    - The tracing continues until the electron's energy drops below a minimum threshold related to the core level energy or it leaves the sample. In the latter case, this backscattered electron trajectory may be traced again when it hits the surface another time.

5.  **Analyzer**:
    - Represents the electron analyzer used to detect Auger electrons emitted from the sample.
    - Configured for CMA ("Cylindrical mirror analyzer) or CHA (Concentric hemi-spherical analyzer) and its orientation relative to the sample's coordinate system.
    - The `Detect` method checks whether an emitted Auger electron's trajectory falls within the analyzer's defined angular acceptance range.
    - Optional features like the aperture restricting the acceptance range can be applied.
    - It accumulates the total detected count of Auger electrons.

## Simulation Results and Visualization

After a simulation run completes, the results are displayed in main form which has several tabs:

### Summary (`pgSummary`)

*   The **Summary Memo** displays a textual summary of the simulation parameters used and the key results.
    - Parameters listed include details about the electron source (energy, beam diameter, focus, incident angle), the analyzer (type, restricted acceptance), and the sample (materials, topography, dimensions,.
    - Results presented include the total number of primary electrons fired, the total number of detected Auger electrons, the total detected intensity (sum of electron weights), and the intensity contributions from different surface regions (top, bottom, sidewall), where applicable based on topography.

### Emission Points (`pgEmissionPoints`)

This tab visualizes and lists the surface points from which Auger electrons were emitted and detected by the analyzer. It has two sub-tabs:

*   **Plot**: A 2D chart displaying the (X, Y) coordinates of the detected Auger electron emission points on the sample surface. The X and Y axes represent the lateral dimensions in micrometers, consistent with the sample coordinate system.

*   **Values**:
    - lists  the raw data for each detected Auger electron, including its sequence number, emission point coordinates (X, Y, Z), and the calculated intensity contributions from different surface regions (Top, Wall, Bottom). It also indicates if the electron was generated by a backscattered electron.

### Trajectories (`pgTrajectories`)

This tab allows visualization of the paths taken by a limited number of primary and scattered electrons within and around the sample. It includes controls for selecting the view and limiting the number of trajectories.

*   **Trajectory Count**: A spin edit to set the maximum number of individual electron trajectories to display. 
*   **Projection**: A radio group to select the 2D projection plane for viewing the 3D trajectories: 'X-Y plane', 'X-Z plane', or 'Y-Z plane'.
*   **Trajectories Chart**: A chart where the electron trajectories are drawn.
    - Each individual trajectory that is recorded by the simulation is added as a separate `TLineSeries` to this chart.

## Potential Applications

The ETrace simulation project, while potentially a research or educational tool, can be applied to solve problems in various fields where electron-sample interactions and surface analysis are important. By simulating the physics of electron-sample interactions and Auger electron emission, ETrace can provide valuable insights that complement experimental work.

Possible applications include:

1.  **Auger Electron Spectroscopy (AES) Analysis**:
    - **Interpreting Spectra from Topographical Samples**: Real-world samples often have complex 3D features (lines, holes, steps, roughness). The intensity of Auger electrons detected from such features is heavily influenced by the local surface orientation relative to the electron beam and the analyzer, as well as by electron scattering within the material. ETrace can simulate the Auger signal from known geometries and material compositions under specific experimental conditions (beam energy, tilt, analyzer type/angle), helping experimentalists understand how topography affects measured intensities and aiding in accurate compositional analysis.
    - **Quantitative Analysis**: By comparing simulated Auger intensities from different regions of a topographical sample to experimental line scans or maps, it may be possible to perform more accurate quantitative elemental analysis than is possible with models that assume flat surfaces.
    - **Signal Origin**: Visualizing electron trajectories and emission points helps understand *where* the detected Auger electrons are originating from, which is crucial for interpreting spatial resolution and depth sensitivity in AES measurements on structured samples.

2.  **Scanning Electron Microscopy (SEM)**:
    - **Image Contrast Interpretation**: While ETrace focuses on Auger electrons, the underlying simulation of primary and scattered electron trajectories is relevant to SEM imaging. The yield of backscattered electrons, for instance, contributes significantly to SEM image contrast, especially for compositional and topographical contrast. ETrace's ability to track primary electron paths and identify where they exit the sample could provide insight into backscattering yield variations on different topographies and materials.
    - **Electron-Beam Induced Effects**: Understanding the penetration depth and scattering volume of the primary electron beam is important for minimizing beam-induced damage or charging in sensitive samples. The trajectory simulations provide a visual representation of the interaction volume.

3.  **Electron Beam Lithography**:
    - **Proximity Effect**: Electron scattering within the resist layer and backscattering from the substrate are primary causes of the proximity effect in electron beam lithography, where features are overexposed by electrons scattered from adjacent exposed areas. While ETrace's focus is not specifically on energy deposition in resist, the core scattering simulation could potentially be adapted or used to generate scattering kernels that inform proximity effect correction strategies for patterned substrates.

4.  **Materials Science Research**:
    - **Investigating Surface Sensitivity**: Simulating emission from varying depths and materials can help researchers understand and visualize the surface sensitivity of AES for specific material systems and electron energies.
    - **Novel Materials and Structures**: ETrace provides a flexible platform to investigate electron interactions with new materials (once their relevant parameters are added to the `TMaterialsList`) or complex layered/structured systems.

5.  **Education and Training**:
    - **Visualization Tool**: The trajectory visualization is a powerful educational tool for demonstrating the principles of electron scattering (elastic and inelastic), energy loss, interaction volume, and the origin of Auger electrons in solids. It can help students grasp complex concepts in surface science, materials analysis, and electron microscopy.

By providing a computational model of electron-sample interactions, ETrace enables predictive simulations that can guide experimental design, aid in data interpretation, and deepen the understanding of fundamental electron-solid physics relevant to various scientific and technological fields.

## Building and Running

The ETrace project is developed using Free Pascal and Lazarus. To build and run the application from source, you need to have these tools installed on your Ubuntu system (or other supported platforms). All required libraries are included in the Lazarus distribution.

1.  **Open the Project in Lazarus**:
    - Launch the Lazarus IDE.
    - Go to `Project -> Open Project File...`.
    - Navigate to the directory where you have the ETrace source code and select the main project file, `etracer.lpr`.

2.  **Configure Project Settings (if necessary)**:
    - Lazarus should automatically configure the project based on the `.lpr` file.
    - Ensure the compiler path is correctly set (usually detected automatically).
    - The project requires the `tachartlazaruspkg` and `lazcontrols` packages. Check the project dependencies in `Project -> Project Options... -> Packages`. If any required package is not installed in your Lazarus environment, you may need to install it via `Package -> Install/Uninstall Packages...`. These packages are usually available through the Lazarus Online Package Manager (OPM) or your distribution's repositories.
    - The units `et_Main`, `et_Math`, `et_Global`, `et_Objects`, and `et_Sim` should be automatically included as they are in the project's source path or explicitly listed in the `.lpr` file's `uses` clause.

4.  **Build the Project**:
    - Go to `Run -> Build` or press `Shift+F9`.
    - Lazarus will compile the Pascal units and link the executable. Any compilation errors will be shown in the Messages window.

5.  **Run the Application**:
    - Go to `Run -> Run` or press `F9`.
    - Lazarus will execute the compiled application.

The executable file (`etracer` on Linux) will be created in the project's main directory or a designated output directory configured in the Project Options.

The project also includes unit tests located in the `unit_tests/` directory (`unit_tests/et_tests.lpr`, `unit_tests/etmathtests.pas`, `unit_tests/etsampletests.pas`). These tests are designed to be run using a GUI test runner (`TGuiTestRunner`), likely included with Lazarus or the fpcunit package. You can typically run these tests by opening `unit_tests/et_tests.lpr` in Lazarus and running that project.



## Source Code Structure

The ETrace project is organized into several files and directories, promoting modularity and separation of concerns:

*   `etracer.lpr`: The main project file for the Lazarus IDE. It defines the program entry point (`begin ... end.`) and lists the primary units used directly by the application's main program, specifically the LCL interface units and the main form unit (`et_Main`).
*   `README.md`: The top-level README file (this documentation).
*   `src/`: This directory contains the core Pascal units (`.pas`) and the Lazarus form definition files (`.lfm`).
    *   `src/et_global.pas`: Defines global constants, enumerations (`TDataType`, `TTopoType`, `TAnalyzerType`, `TNormIntensType`, `TProjection`, `TStepDir`), fundamental record types (`TVector3`, `TRay`, `TSimParams`), and global variables (`etError`, `SimParams`). It establishes the basic data structures and global state for the simulation.
    *   `src/et_math.pas`: Contains mathematical functions and vector operations (`TVector3` and `TRay` operators and functions) essential for 3D geometry and physics calculations. This includes coordinate conversions (Cartesian, Spherical, Cylindrical) and geometric intersection routines (`rayXplane`, `rayXcyl`). It also provides functions for solving quadratic equations and generating random numbers with specific distributions (`Random_Gauss`, `Random_Cos`).
    *   `src/et_objects.pas`: Defines the main object-oriented components of the simulation: `TElectron` (record), `TAugerElectron` (record), `TElectronSource` (class for beam generation), `TAnalyzer` (class for electron detection), `TMaterialParams` (class for material properties data storage), `TMaterialsList` (class for managing material parameters), `TMaterial` (class for material properties and interaction calculations), and the `TSample` class hierarchy (`TSample` - abstract base, `TContactHole`, `TStripe`, `TStep` - concrete geometries). This unit encapsulates the physics models and geometric definitions.
    *   `src/et_sim.pas`: Implements the core simulation engine logic within the `TSimulation` class. It orchestrates the Monte Carlo tracing of electron trajectories (`CalcTrajectory`, `TraceElectron`), manages the interaction loop, handles material changes based on position, and uses the `TAnalyzer` and `TSample` objects. It defines `TTrajectoryPoint` (record) and `TTrajectory` (array of points) for storing path data and defines event handler types (`TCancelEvent`, `TDetectionEvent`, `TTrajectoryCompleteEvent`).
    *   `src/et_main.pas`: The main form unit. It defines the `TMainForm` class, which manages the graphical user interface (GUI) defined in `et_main.lfm`. It contains the event handlers for all GUI controls (buttons, spin edits, combo boxes, etc.), reads simulation parameters from the GUI (`GUIToParams`), initializes and runs the `TSimulation` object (`RunSimulation`), handles simulation progress and events, and updates the GUI with results (summary, emission points list and chart, trajectory chart). It also handles saving and loading parameters and GUI settings (`LoadParamsFromCfg`, `SaveParamsToCfg`, `ReadIni`, `WriteIni`).
    *   `src/et_main.lfm`: The Lazarus Form file defining the visual layout, properties, and event handler assignments for the `TMainForm` GUI. This is a non-human-readable text representation generated by the Lazarus IDE.
    *   `src/et_params.pas`: A unit that appears to be related to simulation parameters, defining some global variables (`MaxEl`, `TraceName`, `EmPtFName`, etc.) and some unused constants. Based on the provided source, parameter management and configuration file handling are primarily done in `et_main.pas` using the `TSimParams` record and INI files. This unit's role in the currently provided code is minimal or vestigial.
*   `unit_tests/`: This directory contains the unit tests for the project, using the fpcunit testing framework.
    *   `unit_tests/et_tests.lpr`: The main project file for compiling and running the unit tests, likely using a GUI test runner (`TGuiTestRunner`).
    *   `unit_tests/etmathtests.pas`: Contains specific unit tests for the mathematical functions implemented in `et_math.pas`, such as vector operations and intersection calculations (`TestVectorMath`, `TestIntersection_RayPlane`).
    *   `unit_tests/etsampletests.pas`: Contains unit tests for the sample geometry intersection logic implemented in the `TSample` derived classes, specifically testing `Intersection` for `TContactHole` (`TestIntersection_ContactHole`) and `TStripe` (`TestIntersection_Stripe`).

This structure promotes modularity and separation of concerns, with global definitions, mathematical utilities, physical objects, simulation logic, and the user interface residing in distinct units.
