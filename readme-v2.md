
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

The core simulation logic is implemented in Pascal units, while the GUI is built using the Lazarus Component Library (LCL), as seen in `etracer.lpr` and `src/et_main.pas`. The `.lfm` file (`src/et_main.lfm`) describes the layout and properties of the GUI elements.

## Core Concepts and Data Structures

Understanding the fundamental data types and concepts is crucial for comprehending the simulation logic. This section details the key records and types defined in `et_global.pas` and used throughout the project.

*   **Float**: Defined as `double`. This specifies that floating-point numbers are stored with double precision, important for numerical accuracy in the simulation calculations.

*   **TDataType**: An enumeration (`(dtNone, dtTraj, dtEmPts)`) used to classify different types of simulation data, such as trajectories (`dtTraj`) and emission points (`dtEmPts`).

*   **TTopoType**: An enumeration (`(ttNone, ttContactHole, ttStripe, ttStep)`) representing the different sample topographies supported by the simulation.
    - `ttContactHole`: Models a contact hole geometry.
    - `ttStripe`: Models a long stripe or line.
    - `ttStep`: Models a step-like topography.

*   **TAnalyzerType**: An enumeration (`(atNone, atCMA, atCHA)`) defining the types of electron analyzers that can be simulated.
    - `atCMA`: Cylindrical Mirror Analyzer.
    - `atCHA`: Concentric Hemispherical Analyzer.

*   **TNormIntensType**: An enumeration (`(niRaw, niPrimEl, niArea)`) related to intensity normalization methods, although its usage might be limited or specific within the current code.

*   **TProjection**: An enumeration (`(XYproj, XZproj, YZproj, ThreeD)`) used primarily for selecting the projection plane for visualizing trajectories in the GUI.

*   **TStepDir**: An enumeration (`(sdNone, sdUp, sdDown)`) specifying the direction of a step topography (whether the higher level is 'up' or 'down' relative to the coordinate system).

*   **TVector3**: A record type representing a 3D vector or point in space with `X`, `Y`, and `Z` components of type `Float`. Used extensively for positions, directions, and other vector quantities.

*   **TRay**: A record combining a starting `Point` (TVector3) and a `Dir`ection (TVector3), used to represent the path of an electron.

*   **TMatrix3**: A record representing a 3x3 matrix, defined as an array of three `TVector3`. While declared, its direct usage in the provided code snippets isn't immediately apparent, suggesting it might be used in other parts of the project or is a leftover.

*   **ExtStr**: A short string type (`String[4]`), possibly used for specific fixed-length identifiers or codes.

*   **Error Codes**: Constants like `etOK`, `etIOError`, `etOutOfMemory`, `etAborted` define a simple error reporting mechanism via the `etError` global variable and `GetError` function.

*   **TSimParams**: A comprehensive record containing all the parameters required to define a simulation run. This includes:
    - `zAxis`: Defines the orientation of the sample surface normal.
    - Electron source parameters (`PrimaryEnergy`, `BeamDiameter`, `Focus`, `NumElectrons`).
    - Electron analyzer parameters (`AnalyzerType`, `SectorStart`, `SectorEnd`, `UseHoeslerAperture`).
    - Sample parameters (`SubstrateName`, `LayerName`, `Topography`, `Width`, `Depth`, `LayerThickness`, `StepDir`, `OnlyDirect`, `TiltAngle`).
    The `DefaultSimParams` constant provides a baseline configuration. This record acts as the central hub for transferring settings from the GUI (`et_main.pas`, `et_main.lfm`) to the simulation engine (`et_sim.pas`).

## Simulation Model

The heart of ETrace lies in its simulation engine, primarily implemented in `et_objects.pas` and `et_sim.pas`. It employs a Monte Carlo approach to simulate the complex interactions of electrons with the sample material.

Key aspects of the simulation model include:

1.  **Electron Source (`TElectronSource` in `et_objects.pas`)**:
    - Generates primary electrons with a specified `PrimaryEnergy` and `BeamDiameter`.
    - Electrons are focused towards a defined `Focus` point on the sample surface.
    - The beam can be tilted relative to the sample normal using the `TiltAngle`.
    - The `GenerateElectron` procedure creates individual primary electrons with their initial position (`Ray.Point`) and direction (`Ray.Dir`).

2.  **Electron-Material Interaction (`TMaterial`, `TMaterialParams`, `TMaterialsList` in `et_objects.pas`)**:
    - The simulation requires accurate material properties to model electron interactions. These properties are encapsulated in the `TMaterial` class and managed by the `TMaterialsList`.
    - **`TMaterialParams`**: This record stores the basic physical properties for a material, such as:
        - `Z`: Atomic number (or an effective identifier for compounds).
        - `A`: Atomic or molecular mass.
        - `MassDensity`: Density in grams per cubic centimeter (g/cm³).
        - `CoreLevel`: Binding energy of the relevant core electron shell in keV.
        - `AugerEnergy`: The characteristic energy of the emitted Auger electron in eV.
    - **`TMaterialsList`**: A list (`TFPObjectList`) that acts as a simple database for available `TMaterialParams`. It includes functions (`SearchZ`, `SearchName`) to retrieve material parameters by atomic number or name.
    - **`TMaterial`**: This class represents a specific instance of a material used in the simulation (e.g., the substrate or layer). It is initialized with parameters from `TMaterialParams` and calculates additional derived properties needed for the Monte Carlo simulation (e.g., `NumDensity`, `ScreeningParam`, `RutherfordParam`, `IonizationPotential`, `StoppingPowerParam`, `EscapeDepth`).
        - Key properties derived and used in calculations include: `ElemDensity` (density of the element being analyzed), `EscapeDepth` (mean free path for Auger electrons), `IonizationPotential` (average energy to ionize an atom), `RutherfordParam` and `ScreeningParam` (used in the elastic scattering model), and `StoppingPowerParam` (used in the inelastic scattering model).
    - The `ChangeMaterial` method in `TSample` updates the currently active `TMaterial` instance based on the electron's position relative to material interfaces.
    - **Available Materials**: The `NewMaterialsList` function in `et_objects.pas` initializes the list of materials with the following predefined substances and their parameters:
        | Name   | Z     | Atomic/Molec. Mass (A) | Mass Density (g/cm³) | Core Level (keV) | Auger Energy (eV) |
        |--------|-------|------------------------|----------------------|------------------|-------------------|
        | Al     | 13.0  | 26.0                   | 2.70                 | 1.487            | 1620              |
        | Si     | 14.0  | 28.0                   | 2.33                 | 1.839            | 1617              |
        | SiO2   | 10.1  | 60.0                   | 2.19                 | 1.839            | 1609              |
        | Fe     | 28.0  | 55.8                   | 7.86                 | 0.707            | 702               |
        | Au     | 79.0  | 197.0                  | 19.3                 | 2.291            | 2016              |
        | Cu     | 29.0  | 63.5                   | 8.93                 | 0.932            | 918               |
        | Ni     | 28.0  | 58.7                   | 8.9                  | 0.853            | 846               |
        | Pt     | 78.0  | 195.1                  | 21.5                 | 2.202            | 1961              |
        | Ag     | 47.0  | 107.9                  | 10.5                 | 0.368            | 358               |
        | Cr     | 24.0  | 52.0                   | 6.9                  | 0.574            | 527               |
        | Mo     | 42.0  | 95.9                   | 10.2                 | 2.867            | 2038              |

3.  **Sample Geometry and Interaction (`TSample`, `TContactHole`, `TStripe`, `TStep` in `et_objects.pas`)**:
    - The `TSample` is an abstract base class defining the common interface for sample objects.
    - It manages the `Substrate` and `Layer` materials and the interface position (`zInterface`).
    - The `ChangeMaterial` method in `et_objects.pas` updates the currently active `TMaterial` instance based on the electron's position relative to material interfaces.
    - The core interaction logic is handled by methods that must be implemented by derived classes:
        - `OnSurface`: Determines if a given point lies on the sample surface.
        - `Outside`: Determines if a given point is outside the sample volume.
        - `Intersection`: Calculates the intersection point of an electron ray with the sample surface. This is crucial for determining when an electron enters or leaves the sample.
        - `SurfNormal`: Calculates the normal vector to the surface at a given point, necessary for determining emission angles.
    - **Derived Classes**: `TContactHole`, `TStripe`, and `TStep` provide concrete implementations of the `TSample` interface for specific geometries. They implement the intersection and surface check logic based on their respective shapes (cylinder and planes for ContactHole, planes for Stripe and Step).
    - `EmitAugerEl`: Simulated Auger electron emission occurs when a primary or scattered electron has sufficient energy (`E >= CoreLevelEnergy`) and reaches a surface point (`OnSurface`). The emission direction is uniformly distributed. The probability of an emitted Auger electron reaching the detector is influenced by its emission angle and the material's `EscapeDepth`. `Electron.Weight` quantifies this contribution to the detected signal. Implemented in `et_objects.pas`.
    - `Scatter`: Implements the Monte Carlo step for electron scattering. Based on the current electron `Energy` and the material's properties (`RutherfordScattParams`, `StoppingPowerParam`), it calculates a new direction and energy after traveling a simulated step distance. Implemented in `et_objects.pas`.

4.  **Electron Trajectory Calculation (`TSimulation` in `et_sim.pas`)**:
    - The `TSimulation` class orchestrates the entire process. Its `Execute` method generates the specified number of primary electrons from the `FElectronSource` and calls `TraceElectron` (which in turn calls `CalcTrajectory`) for each primary electron and any subsequent scattered electrons that escape and re-enter the sample.
    - The `CalcTrajectory` procedure (defined in `et_sim.pas`) traces the path of a single electron (primary or scattered) step-by-step within the sample.
    - At each step, it checks if the electron has crossed a material interface (`zIntf`) and updates the material properties accordingly (`ChangeMaterial` in `et_objects.pas`).
    - It checks for intersections with the sample boundary (`FSample.Outside` in `et_objects.pas`) and records the exit point if the electron leaves the sample.
    - If the electron is on the surface and has sufficient energy, it triggers the `EmitAugerEl` process (in `et_objects.pas`).
    - If the electron is inside the material and hasn't exited, its direction and energy is updated via the `FSample.Scatter` method (in `et_objects.pas`).
    - The trajectory points (position and energy) are stored and can be visualized.
    - The tracing continues until the electron's energy drops below a minimum threshold (`EMin` related to core level energies) or it leaves the sample.
    - The `OnTrajectoryComplete` event handler (used in `et_sim.pas`) triggers the creation of visualization data when a trajectory finishes.

5.  **Analyzer (`TAnalyzer` in `et_objects.pas`)**:
    - Represents the electron analyzer used to detect Auger electrons.
    - Configured by `AnalyzerType` (CMA or CHA) and its orientation (`PolarAngle`, `AzimuthAngle`).
    - The `Detect` method checks if an emitted Auger electron's trajectory (`Electron.Ray.Dir`) falls within the analyzer's acceptance angle defined by `FAcc1` and `FAcc2` (cosine values of polar angle range).
    - Optional features like the `UseHoeslerAp` (rejecting electrons emitted at grazing angles to the surface normal) and `Restrict`ed azimuthal acceptance can be applied.
    - It accumulates the total `Detected` count and the sum of detected electron `Intensity` (weights).

## User Interface and Parameters

The ETrace application provides a graphical user interface (GUI) built with Lazarus (described in `src/et_main.lfm`) to configure and run simulations. The main form (`TMainForm` in `src/et_main.pas`) is organized into several sections:

### Electron Gun Parameters (`gbEGun`)

This group box contains controls related to the primary electron beam:

*   **Primary electron count (`sePrimElCount`)**: A spin edit for specifying the total number of primary electrons to simulate. Corresponds to `TSimParams.NumElectrons`.
*   **Primary energy (keV) (`sePrimEnergy`)**: A float spin edit to set the energy of the primary electron beam in keV. Corresponds to `TSimParams.PrimaryEnergy`.
*   **Beam diameter (µm) (`seBeamDiam`)**: A float spin edit for the diameter of the electron beam at the focus point in micrometers. Corresponds to `TSimParams.BeamDiameter`.
*   **Focus X, Y, Z (µm) (`seFocusX`, `seFocusY`, `seFocusZ`)**: Float spin edits to define the (X, Y, Z) coordinates in micrometers where the electron beam is focused on the sample. Corresponds to `TSimParams.Focus`.

### Analyzer Parameters (`gbAnalyzer`)

This group box controls the settings of the simulated electron analyzer:

*   **Type (`cmbAnalyzerType`)**: A combo box to select the analyzer type: 'CMA' (Cylindrical Mirror Analyzer) or 'CHA' (Concentric Hemispherical Analyzer). Corresponds to `TSimParams.AnalyzerType`. The GUI maps 'CMA' to index 0 (corresponding to `atCMA` in `et_global.pas`) and 'CHA' to index 1 (`atCHA`).
*   **Annular aperture (`cbAnnularAperture`)**: A checkbox. When checked, it enables the 'from' and 'to' angle inputs for restricting the azimuthal acceptance of the analyzer. Note that azimuthal restriction is only supported for CMA (`atCMA`). This likely influences the `TSimParams.SectorStart` and `TSimParams.SectorEnd` values implicitly or enables/disables their use.
*   **from, to (deg) (`seSectorFrom`, `seSectorTo`)**: Float spin edits to define the start and end angles (in degrees) for the annular (azimuthal) aperture. Only enabled when 'Annular aperture' is checked and the analyzer type is CMA. These correspond to `TSimParams.SectorStart` and `TSimParams.SectorEnd`.
*   **Hösler aperture (> 80 deg) (`cbUseHoeslerAp`)**: A checkbox to enable the Hösler aperture, which restricts detected electrons to those emitted at angles greater than 80 degrees from the surface normal. Only applicable for CMA. Corresponds to `TSimParams.UseHoeslerAperture`.

### Sample Parameters (`gbSample`)

This group box defines the material and geometry of the sample:

*   **Tilt angle (deg) (`seTiltAngle`)**: A float spin edit to set the tilt angle of the sample relative to the electron beam axis in degrees. Corresponds to `TSimParams.TiltAngle`.
*   **Substrate material (`cmbSubstrate`)**: A combo box listing available substrate materials (e.g., Si, Fe, Au). The list is populated from the `TMaterialsList` in `et_objects.pas`. Corresponds to `TSimParams.SubstrateName`.
*   **Layer material (`cmbLayer`)**: A combo box listing available layer materials. Also populated from `TMaterialsList`. Corresponds to `TSimParams.LayerName`.
*   **Layer thickness (µm) (`seLayerThickness`)**: A float spin edit for the thickness of the layer in micrometers. A value of -1 indicates the layer thickness is equal to the feature depth/height for topographical samples. Corresponds to `TSimParams.LayerThickness`.
*   **Topography (`cmbTopography`)**: A combo box to select the sample geometry: 'Contact hole', 'Stripe', or 'Step'. The GUI maps these to indices 0, 1, and 2, corresponding to `ttContactHole`, `ttStripe`, and `ttStep` respectively. Corresponds to `TSimParams.Topography`.
    - Depending on the selected topography, the labels and enabled state of the 'Depth', 'Width', and 'Direction' controls change (`cmbTopographyChange` procedure in `et_main.pas`).
*   **Depth (µm) (`seDepth`)**: A float spin edit. Represents the depth of the contact hole or the height of the stripe/step in micrometers. Corresponds to `TSimParams.Depth`.
*   **Width (µm) (`seWidth`)**: A float spin edit. Represents the diameter of the contact hole or the width of the stripe in micrometers. This control is disabled for the 'Step' topography. Corresponds to `TSimParams.Width`.
*   **Direction (`sbDirUp`, `sbDirDown`)**: Speed buttons to specify the direction of the step (Up or Down). Only enabled for the 'Step' topography. These correspond to `TSimParams.StepDir`.

### Simulation Control

*   **Run simulation (`btnRunSim`)**: A button to start the simulation. The caption changes to 'Abort' while running, allowing the user to interrupt the simulation.
*   **Trajectory count (`seTrajectories`)**: A spin edit (in the Trajectories tab) to limit the number of individual electron trajectories that are stored and displayed. This helps manage memory and improve performance for large simulations.

The GUI elements are connected to event handlers (e.g., `btnRunSimClick`, `cmbTopographyChange`) in `et_main.pas` which read the values from the controls, populate the global `SimParams` record (`GUIToParams`), trigger the simulation (`RunSimulation`), and update the display based on results and selected options (`ParamsToGUI`, `UpdateCtrlState`). Parameters are saved to and loaded from an INI configuration file (`etracer.cfg`) using `LoadParamsFromCfg` and `SaveParamsToCfg`.



## Simulation Results and Visualization

After a simulation run completes, the results are displayed in the `ResultsPageControl` area of the main form (`et_main.lfm`). This control has several tabs:

### Summary (`pgSummary`)

*   **Summary Memo (`SummaryMemo`)**: This memo displays a textual summary of the simulation parameters used and the key results.
    - Parameters listed include details about the electron source (energy, beam diameter, focus, incident angle), the analyzer (type, restrictions, Hösler aperture), and the sample (materials, topography, dimensions, tilt angle).
    - Results presented include the total number of primary and detected Auger electrons, the total detected intensity, and the intensity contributions from different surface regions (top, bottom, sidewall), where applicable based on topography. This intensity breakdown is calculated by the `EvalIntensities` procedure in `et_main.pas` based on the emission point coordinates and the geometry defined by the `SimParams`.
    - The `DisplaySummary` procedure in `et_main.pas` is responsible for formatting and populating the content of this memo.

### Emission Points (`pgEmissionPoints`)

This tab visualizes and lists the surface points from which Auger electrons were emitted and detected by the analyzer.

*   **Plot (`pgPlot`)**:
    - **Emission Points Chart (`EmissionPointsChart`)**: A 2D chart displaying the (X, Y) coordinates of the emission points on the sample surface. The X and Y axes represent the lateral dimensions in micrometers.
    - **Emission Points Series (`EmissionPointsSeries`)**: A scatter series (points only, no connecting lines) used to draw the individual emission points on the chart.
    - **Emission Points Source (`EmissionPointsSource`)**: A `TUserDefinedChartSource` which provides the data (X, Y coordinates) for the `EmissionPointsSeries`. The `EmissionPointsSourceGetChartDataItem` procedure in `et_main.pas` retrieves the coordinates from the `FEmissionPoints` array.
    - **Horizontal Extent Spin Edits (`seEmissionPointsHorMin`, `seEmissionPointsHorMax`)**: Float spin edits that allow the user to control the visible range of the X-axis on the Emission Points Chart. The chart's Y-axis range is adjusted proportionally to maintain the aspect ratio, as the chart is set to `Proportional = True`. The `EmissionPointsChartExtentChange` procedure handles updates to the chart's extent based on these controls.
    - The simulation collects all detected Auger electron emission points in the `FEmissionPoints` array (`DetectionHandler` in `et_main.pas`).

*   **Values (`pgValues`)**:
    - **Emission Points Memo (`EmissionPointsMemo`)**: A memo displaying the raw data for each detected Auger electron, including its sequence number, emission point coordinates (X, Y, Z), and the calculated intensity contributions from different surface regions (Top, Wall, Bottom). It also indicates if the electron was generated by a backscattered electron ('X' in the 'BkSc' column).
    - The `DetectionHandler` procedure in `et_main.pas` formats and adds each detected electron's data as a new line in this memo.

### Trajectories (`pgTrajectories`)

This tab allows visualization of the paths taken by a limited number of primary and scattered electrons within and around the sample.

*   **Trajectory count (`seTrajectories`)**: A spin edit (in the Trajectories tab) to limit the number of individual electron trajectories that are stored and displayed. This helps manage memory and improve performance for large simulations.
*   **Projection Radio Group (`rgProjection`)**: A radio group to select the projection plane for viewing the trajectories: 'X-Y plane', 'X-Z plane', or 'Y-Z plane'. Corresponds to the `TProjection` enumeration. The `rgProjectionClick` procedure updates the chart axes titles and resets the trajectory series sources to redraw the trajectories in the new projection.
*   **Trajectories Chart (`TrajectoriesChart`)**: A chart where the electron trajectories are drawn.
    - Each individual trajectory recorded by the simulation is added as a separate `TLineSeries` to this chart.
    - Each series uses a `TUserDefinedChartSource` (`TrajectoryGetChartDataItemHandler` in `et_main.pas`) to retrieve the trajectory points (`FTrajectories` array). The `OnTrajectoryComplete` event handler in `TSimulation` triggers the creation of these series when a trajectory finishes.
    - The chart's axes titles change based on the selected `rgProjection` (`TrajectoriesChart.LeftAxis.Title.Caption`, `TrajectoriesChart.BottomAxis.Title.Caption`).
    - **Horizontal Extent Spin Edits (`seTrajectoryHorMin`, `seTrajectoryHorMax`)**: Similar to the Emission Points chart, these controls allow adjusting the visible range of the horizontal axis. `TrajectoriesChartExtentChange` handles these updates.
    - **`TrajectoriesChartAfterDraw`**: This procedure draws the sample geometry outline (contact hole, stripe, or step) and the initial electron beam path on top of the trajectories, providing context for the simulation. The geometry outline is drawn in red, and the electron beam in blue.

The `ProgressBar` on the `ParamsPanel` provides visual feedback on the simulation progress by showing the number of primary electrons processed (`CancelHandler` in `et_main.pas`).



## Building and Running

The ETrace project is developed using Free Pascal and Lazarus. To build and run the application from source, you need to have these tools installed on your Ubuntu system (or other supported platforms).

1.  **Install Free Pascal and Lazarus**:
    If you don't have them installed, you can typically install them using your system's package manager:
    ```bash
    sudo apt update
    sudo apt install fpc laz Lazarus
    ```

2.  **Open the Project in Lazarus**:
    - Launch the Lazarus IDE.
    - Go to `Project -> Open Project File...`.
    - Navigate to the directory where you have the ETrace source code and select the main project file, `etracer.lpr`.

3.  **Configure Project Settings (if necessary)**:
    - Lazarus should automatically configure the project based on the `.lpr` file.
    - Ensure the compiler path is correctly set (usually detected automatically).
    - The project seems to use standard LCL components and should compile without extra packages beyond `tachartlazaruspkg` and `lazcontrols`. Check the project dependencies in `Project -> Project Options... -> Packages`. If any required package is not installed in your Lazarus environment, you may need to install it via `Package -> Install/Uninstall Packages...`.
    - The units `et_Main`, `et_Math`, `et_Global`, `et_Objects`, and `et_Sim` should be automatically included as they are in the project's source path or explicitly listed in the `.lpr` file's `uses` clause.

4.  **Build the Project**:
    - Go to `Run -> Build` or press `Shift+F9`.
    - Lazarus will compile the Pascal units and link the executable. Any compilation errors will be shown in the Messages window.

5.  **Run the Application**:
    - Go to `Run -> Run` or press `F9`.
    - Lazarus will execute the compiled application.

The executable file (`etracer` on Linux) will be created in the project's main directory or a designated output directory configured in the Project Options.

The project also includes unit tests (`unit_tests/et_tests.lpr`, `unit_tests/etmathtests.pas`, `unit_tests/etsampletests.pas`). These can be run within Lazarus or potentially compiled and run separately, though the provided `.lpr` file suggests a GUI test runner (`TGuiTestRunner`).

## Potential Applications

The ETrace simulation project, while potentially a research or educational tool, can be applied to solve problems in various fields where electron-sample interactions and surface analysis are important.

Possible applications include:

1.  **Scanning Electron Microscopy (SEM) and Auger Electron Spectroscopy (AES)**:
    - Understanding and interpreting AES spectra from samples with complex topographies (contact holes, lines, steps). The simulation can help predict how the signal intensity from different elements changes depending on the electron beam's position and the sample's geometry and material composition.
    - Optimizing SEM imaging parameters (beam energy, tilt angle) to enhance contrast or reduce charging effects by understanding electron trajectories and scattering.
    - Quantifying the elemental composition of small features or rough surfaces by comparing experimental AES data to simulated results for known geometries and materials.

2.  **Nanofabrication and Microelectronics**:
    - Simulating electron beam lithography processes to understand electron scattering effects (proximity effect) in resist materials and substrates, which is critical for designing high-resolution patterns. Although ETrace is focused on backscattered/Auger electrons rather than transmitted electrons through a resist, the scattering core could be adapted or provide insight.
    - Analyzing defects in semiconductor devices. The simulation can help understand how electron beams interact with complex layered structures and topographical features, aiding in defect localization and characterization using techniques like AES or Energy Dispersive X-ray Spectroscopy (EDX, though EDX simulation is not currently part of ETrace).
    - Designing and optimizing the geometry of micro/nano-structures to minimize unwanted electron scattering or maximize desired signal collection in electron-based analysis techniques.

3.  **Materials Science**:
    - Investigating the surface sensitivity of AES for different materials and electron energies.
    - Studying the effect of surface roughness or specific topographical features on electron emission and detection.
    - Comparing the performance of different analyzer types (CMA vs CHA) for specific sample geometries or analysis goals.

4.  **Research and Education**:
    - Serving as a valuable educational tool to visualize electron trajectories and scattering processes in materials, helping students understand the fundamental principles of electron-solid interactions and surface analysis techniques.
    - Providing a platform for researchers to test hypotheses about electron behavior in novel materials or complex structures.
    - The modular nature of the code allows for extension to include other interaction processes, sample geometries, or analysis techniques.

By simulating the physics of electron-sample interactions, ETrace can provide insights that are difficult or impossible to obtain solely through experimental means, helping to optimize experimental parameters, interpret complex data, and understand the underlying physical phenomena in micro and nanoscale analysis.

## Source Code Structure

An overview of the project's file and directory structure to help developers navigate the codebase.

*   `etracer.lpr`: The main project file for Lazarus. It defines the program entry point and lists the primary units used by the application, particularly the main form unit (`et_main.pas`).
*   `README.md`: The top-level README file (this file).
*   `src/`: This directory contains the core Pascal units and the Lazarus form definition files.
    *   `src/et_global.pas`: Defines global constants, types, and variables used throughout the project, including fundamental data structures like `TVector3`, `TRay`, and `TSimParams`.
    *   `src/et_math.pas`: Contains mathematical functions and vector operations (`TVector3` and `TRay` operators and functions) used in the simulation, including coordinate system conversions and intersection calculations (`rayXplane`, `rayXcyl`).
    *   `src/et_objects.pas`: Defines the main object classes representing the physical components of the simulation: `TElectronSource`, `TAnalyzer`, `TMaterial`, and the `TSample` hierarchy (`TContactHole`, `TStripe`, `TStep`). It also includes the `TMaterialsList` data structure.
    *   `src/et_sim.pas`: Implements the core simulation logic. It contains the `TSimulation` class, which orchestrates the Monte Carlo tracing of electron trajectories (`CalcTrajectory`) and manages the interactions between the electron source, sample, and analyzer. It defines the `TTrajectory` type used to store path points.
    *   `src/et_main.pas`: The main form unit. It defines the `TMainForm` class, which handles the graphical user interface (GUI). It contains the event handlers for the GUI controls, reads parameters from the GUI, triggers the simulation (`RunSimulation`), and displays the results. It uses components defined in `et_main.lfm`. Parameters are saved to and loaded from an INI configuration file (`etracer.cfg`). Note that while `et_params.pas` contains a constant like `CFG_FILE_NAME = 'CALC_ET.CFG'`, the active configuration file name used by the GUI is defined by the constant `CFG_FILE_NAME = 'etracer.cfg'` from `et_Global.pas` and handled within `et_main.pas`. The constants in `et_params.pas` appear unused.
    *   `src/et_main.lfm`: The Lazarus Form file defining the visual layout and properties of the `TMainForm` GUI.
    *   `src/et_params.pas`: This unit seems vestigial in the current project structure. Based on the provided source, its contents (constants and an empty implementation section) appear largely unused, with primary parameter management handled by the `TSimParams` record (`et_Global.pas`) and the loading/saving logic in `et_main.pas`.
*   `unit_tests/`: This directory contains the unit tests for the project.
    *   `unit_tests/et_tests.lpr`: The main project file for running the unit tests, likely using a GUI test runner.
    *   `unit_tests/etmathtests.pas`: Contains unit tests specifically for the mathematical functions defined in `et_Math.pas`.
    *   `unit_tests/etsampletests.pas`: Contains unit tests for the sample geometry intersection logic implemented in the `TSample` derived classes (`TContactHole`, `TStripe`).

This structure separates global definitions, mathematical utilities, physical objects, simulation core logic, and the user interface into distinct units, promoting modularity and maintainability.

## Building and Running

The ETrace project is developed using Free Pascal and Lazarus. To build and run the application from source, you need to have these tools installed on your Ubuntu system (or other supported platforms).

1.  **Install Free Pascal and Lazarus**:
    If you don't have them installed, you can typically install them using your system's package manager:
    ```bash
    sudo apt update
    sudo apt install fpc laz Lazarus
    ```

2.  **Open the Project in Lazarus**:
    - Launch the Lazarus IDE.
    - Go to `Project -> Open Project File...`.
    - Navigate to the directory where you have the ETrace source code and select the main project file, `etracer.lpr`.

3.  **Configure Project Settings (if necessary)**:
    - Lazarus should automatically configure the project based on the `.lpr` file.
    - Ensure the compiler path is correctly set (usually detected automatically).
    - The project seems to use standard LCL components and should compile without extra packages beyond `tachartlazaruspkg` and `lazcontrols`. Check the project dependencies in `Project -> Project Options... -> Packages`. If any required package is not installed in your Lazarus environment, you may need to install it via `Package -> Install/Uninstall Packages...`.
    - The units `et_Main`, `et_Math`, `et_Global`, `et_Objects`, and `et_Sim` should be automatically included as they are in the project's source path or explicitly listed in the `.lpr` file's `uses` clause.

4.  **Build the Project**:
    - Go to `Run -> Build` or press `Shift+F9`.
    - Lazarus will compile the Pascal units and link the executable. Any compilation errors will be shown in the Messages window.

5.  **Run the Application**:
    - Go to `Run -> Run` or press `F9`.
    - Lazarus will execute the compiled application.

The executable file (`etracer` on Linux) will be created in the project's main directory or a designated output directory configured in the Project Options.

The project also includes unit tests (`unit_tests/et_tests.lpr`, `unit_tests/etmathtests.pas`, `unit_tests/etsampletests.pas`). These can be run within Lazarus or potentially compiled and run separately, though the provided `.lpr` file suggests a GUI test runner (`TGuiTestRunner`).

## Potential Applications

The ETrace simulation project, while potentially a research or educational tool, can be applied to solve problems in various fields where electron-sample interactions and surface analysis are important.

Possible applications include:

1.  **Scanning Electron Microscopy (SEM) and Auger Electron Spectroscopy (AES)**:
    - Understanding and interpreting AES spectra from samples with complex topographies (contact holes, lines, steps). The simulation can help predict how the signal intensity from different elements changes depending on the electron beam's position and the sample's geometry and material composition.
    - Optimizing SEM imaging parameters (beam energy, tilt angle) to enhance contrast or reduce charging effects by understanding electron trajectories and scattering.
    - Quantifying the elemental composition of small features or rough surfaces by comparing experimental AES data to simulated results for known geometries and materials.

2.  **Nanofabrication and Microelectronics**:
    - Simulating electron beam lithography processes to understand electron scattering effects (proximity effect) in resist materials and substrates, which is critical for designing high-resolution patterns. Although ETrace is focused on backscattered/Auger electrons rather than transmitted electrons through a resist, the scattering core could be adapted or provide insight.
    - Analyzing defects in semiconductor devices. The simulation can help understand how electron beams interact with complex layered structures and topographical features, aiding in defect localization and characterization using techniques like AES or Energy Dispersive X-ray Spectroscopy (EDX, though EDX simulation is not currently part of ETrace).
    - Designing and optimizing the geometry of micro/nano-structures to minimize unwanted electron scattering or maximize desired signal collection in electron-based analysis techniques.

3.  **Materials Science**:
    - Investigating the surface sensitivity of AES for different materials and electron energies.
    - Studying the effect of surface roughness or specific topographical features on electron emission and detection.
    - Comparing the performance of different analyzer types (CMA vs CHA) for specific sample geometries or analysis goals.

4.  **Research and Education**:
    - Serving as a valuable educational tool to visualize electron trajectories and scattering processes in materials, helping students understand the fundamental principles of electron-solid interactions and surface analysis techniques.
    - Providing a platform for researchers to test hypotheses about electron behavior in novel materials or complex structures.
    - The modular nature of the code allows for extension to include other interaction processes, sample geometries, or analysis techniques.

By simulating the physics of electron-sample interactions, ETrace can provide insights that are difficult or impossible to obtain solely through experimental means, helping to optimize experimental parameters, interpret complex data, and understand the underlying physical phenomena in micro and nanoscale analysis.

