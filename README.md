# ETrace

![grafik](https://github.com/wp-xyz/ETrace/assets/30792460/04a6d097-3d41-467a-aa41-2d77e5c86cc9)

## Project Overview

ETrace is a Free Pascal project designed for simulating electron trajectories and calculating Auger electron intensity from various sample geometries under electron beam irradiation. It is intended for researchers and developers interested in electron microscopy and surface analysis techniques like Auger Electron Spectroscopy (AES).

The project provides a framework for defining electron sources, sample geometries (including simple flat surfaces, contact holes, stripes, and steps), and electron analyzers. It traces individual primary and scattered electron trajectories, simulates Auger electron generation, and calculates the detected signal.

## Real-World Applications and Problem Solving

ETrace, as a simulation tool for electron-sample interactions and Auger electron emission, can be applied to various practical problems in materials science, surface physics, and analytical microscopy. It provides a virtual laboratory for investigating phenomena relevant to techniques like Auger Electron Spectroscopy (AES) and Scanning Electron Microscopy (SEM).

*   **Understanding and Interpreting AES Spectra**: Simulate electron trajectories, energy loss, scattering events, Auger generation, and escape from specific material compositions and structures. This helps in understanding how the detected Auger signal intensity and shape are influenced by fundamental processes. Comparing simulated results with experimental AES spectra can aid in deconvoluting complex spectra or identifying contributions from different depths or sample regions.
*   **Optimizing Experimental Parameters**: Investigate the effect of changing electron beam parameters (e.g., primary energy using `TSimParams.PrimaryEnergy`, beam diameter using `TSimParams.BeamDiameter`) or analyzer geometry (e.g., polar/azimuthal angles via analyzer configuration, sector angles using `TSimParams.SectorStart`/`SectorEnd`) on the detected Auger signal for a given sample. This allows researchers to optimize their experimental setup for maximum signal-to-noise ratio, sensitivity to specific elements, or enhanced spatial resolution.
*   **Analyzing Topographical Effects on Signal and Resolution**: Study how non-flat surface topographies (simulated using `TContactHole`, `TStripe`, `TStep` classes and `TSimParams.Topography`, `Width`, `Depth`, `LayerThickness`, `StepDir`) affect the electron trajectories, interaction volume, Auger generation sites, and electron escape paths. This is crucial for quantitative analysis of samples with complex surfaces where topographical shadowing, signal reabsorption, or altered electron scattering can significantly distort the detected signal and limit spatial resolution. ETrace can help predict these effects and guide data interpretation or correction methods.
*   **Spatial Resolution Studies**: By simulating emission points (`FEmissionPoints`) for a focused beam, one can estimate the effective area on the sample surface from which Auger electrons are collected by the analyzer. This provides insight into the achievable spatial resolution of the AES technique for different sample types and topographies.
*   **Layer Thickness and Depth Profiling Insights**: Simulate the signal contribution from different depths within a layered sample (`TSimParams.LayerThickness`, `Layer`, `Substrate`) or from features of varying height (`TStep.Height`, `TContactHole.Depth`, `TStripe.Height`). This helps build an understanding of the relationship between signal intensity and depth, which is foundational for quantitative depth profiling using ion sputtering in conjunction with AES.
*   **Educational Tool**: The ability to visualize electron trajectories (`TrajectoriesChart`) and emission points (`EmissionPointsChart`) provides a powerful educational tool for students and researchers to gain intuitive understanding of the complex physics involved in electron-solid interactions (elastic scattering modeled, inelastic scattering via stopping power, Auger generation) and signal detection in surface analysis.
*   **Foundation for Method Development**: The modular structure of ETrace allows it to serve as a basis for developing and testing new simulation algorithms, scattering models, or methods for analyzing simulation data. The separation of core simulation logic (`et_sim`, `et_objects`) from the GUI (`et_main`) also suggests the potential for extending the project with a command-line interface for automated simulations or integration into larger computational workflows, although this is not an existing feature based on the provided interfaces.

## How to Use ETrace

Based on the `et_main.pas` unit, the primary way to interact with ETrace is through its graphical user interface (GUI).

1.  **Launch the Application**: Execute the compiled ETrace application executable (refer to the "Building the Project" section for compilation instructions).
2.  **Set Simulation Parameters**: Use the various controls on the `TMainForm` window to configure the simulation settings. Parameters are logically grouped into sections (E-Gun, Analyzer, Sample) and include details about the electron beam energy and size, analyzer type and acceptance angles, sample material composition (Substrate and Layer), sample geometry type (Topography) and dimensions, tilt angle, and the number of electrons to simulate.
3.  **Load/Save Parameters**: The GUI includes functionality (via `LoadParamsFromCfg`, `SaveParamsFromCfg`) to load predefined parameter sets from or save the current configuration to the `etracer.cfg` configuration file. Look for corresponding buttons or menu items on the form, likely in a "File" menu or a dedicated panel. This feature is useful for saving frequently used setups and ensuring reproducibility.
4.  **Run Simulation**: Click the "Run Simulation" button (likely labeled "Run" or "Start Simulation"). This action triggers the `btnRunSimClick` event handler in `TMainForm`, which in turn calls `RunSimulation` to gather parameters (`GUIToParams`), create and configure the `TSimulation` object, connect event handlers, and start the simulation loop (`TSimulation.Execute`).
5.  **Monitor Progress**: While the simulation is running, the `ProgressBar` on the form will likely indicate the simulation's progress (e.g., showing the percentage of primary electrons processed or the count of completed electrons). The `SummaryMemo` might display real-time updates on the number of detected electrons or other key statistics as reported by the simulation's event handlers.
6.  **View Results**: Once the simulation is complete or as it progresses (depending on implementation details), results are displayed in dedicated areas of the form, typically managed by the `ResultsPageControl`.
    *   **Summary**: Check the `pgSummary` tab and the `SummaryMemo` for a text-based summary of the simulation parameters, duration, and key outcomes like the total number of detected electrons or calculated intensities (`TopInt`, `WallInt`, `BotInt` from `et_params.pas` might be presented here).
    *   **Emission Points**: Navigate to the `pgEmissionPoints` tab. The `EmissionPointsChart` will display a scatter plot showing the (X, Y) or other projected coordinates on the sample surface where detected Auger electrons escaped. The data for this chart is collected in the `FEmissionPoints` array via the `DetectionHandler`. The `EmissionPointsMemo` might list the raw coordinates of these points.
    *   **Trajectories**: Navigate to the `pgTrajectories` tab. The `TrajectoriesChart` will visualize the paths taken by simulated electrons. The `rgProjection` radio group allows you to select the desired 2D projection (XY, XZ, YZ) or potentially a pseudo-3D view. The number of trajectories plotted might be limited by the value in `seTrajectories`. Trajectory data is collected in the `FTrajectories` array via the `TrajectoryCompleteHandler`.
    *   **Values**: The `pgValues` tab sheet exists but its content is not detailed in the provided interfaces. It might be intended for displaying raw numerical data from the simulation results in a grid or table, possibly including energy distributions or detailed lists of detected events.

The GUI provides a user-friendly interface to set up complex simulation scenarios, run the simulation, and visually explore the resulting electron behavior and signal generation.

By enabling users to define and simulate interactions within controlled conditions, ETrace provides a valuable tool for forward modeling, experimental design, data interpretation, and fundamental research in electron spectroscopy and microscopy.

## Core Concepts

The simulation relies on several fundamental data structures and concepts defined primarily in `et_global.pas`, `et_objects.pas`, and `et_sim.pas`.

### Global Definitions (`et_global.pas`)

This unit defines global types, constants, and variables used throughout the project.

*   **`Float`**: An alias for `double`, used for floating-point calculations.
*   **`TVector3`**: Represents a 3D vector with `X`, `Y`, and `Z` components. Used for positions, directions, etc.
    ```pascal
    TVector3 = record
      X, Y, Z: float;
    end;
    ```
*   **`TRay`**: Represents a ray in 3D space with a `Point` (start) and a `Dir` (direction).
    ```pascal
    TRay = record
      Point : TVector3;       { start point of ray }
      Dir   : TVector3;       { ray direction      }
    END;
    ```
*   **`TSimParams`**: A record holding all configurable parameters for a simulation run. This includes electron source properties (energy, diameter, focus, count), analyzer settings (type, sector, aperture), and sample properties (material, topography, dimensions, tilt).
    ```pascal
    TSimParams = record
      // ... fields for general, electron source, electron analyzer, and sample parameters ...
    end;
    ```
*   **`DefaultSimParams`**: A constant `TSimParams` record providing default values.
*   **`etError`**: A global integer variable indicating the last encountered error code (`etOK`, `etIOError`, `etOutOfMemory`, `etAborted`).
*   **`TDataType`, `TTopoType`, `TAnalyzerType`, `TNormIntensType`, `TProjection`, `TStepDir`**: Enumerated types defining various simulation options and classifications.

### Simulation Objects (`et_objects.pas`)

This unit defines the classes representing the physical entities involved in the simulation.

*   **`TElectron`**: Represents a primary or scattered electron. Includes its `Ray` (position and direction) and `Weight` (used for statistical weighting, e.g., in Monte Carlo).
*   **`TAugerElectron`**: Represents an Auger electron. Similar to `TElectron` but also includes a `GenByBkscEl` flag indicating if it was generated by a backscattered electron.
*   **`TMaterialParams`**: Stores fundamental properties of a material element or compound (Z, A, Mass Density, Core Level Energy, Auger Energy).
*   **`TMaterialsList`**: A collection (`TFPObjectList`) of `TMaterialParams`.
*   **`TMaterial`**: Represents a material used in the simulation, derived from `TMaterialParams` but calculating derived properties needed for scattering and stopping power calculations (e.g., `ElemDensity`, `EscapeDepth`, `IonizationPotential`, `RutherfordParam`, `ScreeningParam`, `StoppingPowerParam`). Includes methods like `CalcAugerCrossSection`, `CalcAugerEscapeDepth`, `CalcStoppingPower`, `RutherfordScattParams`.
*   **`TElectronSource`**: Represents the electron beam source. Configured with energy, beam diameter, focus point, and axis. Can `GenerateElectron`.
*   **`TAnalyzer`**: Represents the electron analyzer. Configured with type and angles. Has a `Detect` method to check if an Auger electron enters the analyzer aperture.
*   **`TSample`**: An abstract base class for sample geometry. It holds references to the `Substrate` and `Layer` materials. Key virtual/abstract methods define the sample shape and interaction:
    *   `Intersection(Ray, Point, FromOutside)`: Calculates where a ray intersects the sample surface.
    *   `OnSurface(Point)`: Checks if a point is on the sample surface.
    *   `Outside(Point)`: Checks if a point is outside the sample volume.
    *   `SurfNormal(Point, Normal)`: Returns the surface normal vector at a given point.
    *   `Scatter(Electron, E)`: Simulates electron scattering within the material.
    *   `EmitAugerEl(Point, E, Electron)`: Simulates Auger electron emission.
*   **`TContactHole`, `TStripe`, `TStep`**: Concrete implementations of `TSample` for specific predefined geometries. They implement the abstract methods defined in `TSample` according to their shape.

### Simulation Engine (`et_sim.pas`)

This unit contains the main simulation logic.

*   **`TSimulation`**: The core class for running a simulation. It encapsulates the `TElectronSource`, `TSample`, and `TAnalyzer` objects based on the input `TSimParams`.
    *   `Execute(AMaxPrimElectrons)`: Starts and runs the simulation for a specified number of primary electrons. Returns an integer error code (`etOK` or other `etError` values).
    *   `CalcTrajectory(...)`: Calculates the trajectory of a single electron step by step, handling scattering events and checking for material boundaries. It processes interactions within the material based on the `TSample.Scatter` and `TSample.EmitAugerEl` methods.
    *   `TraceElectron(...)`: Manages the simulation of a single primary electron, including generating secondary electrons (like Auger) and tracing their paths.
    *   Event Properties (`OnCancel`, `OnDetection`, `OnTrajectoryComplete`): These event properties allow external code (specifically the `TMainForm` GUI in `et_main.pas`) to register callback procedures. When the corresponding event occurs within the simulation engine, the registered procedure is called, allowing the GUI to receive asynchronous updates without blocking the user interface. For example, `OnCancel` allows the GUI to signal the simulation to stop, `OnDetection` provides data for each detected Auger electron (handled by `TMainForm.DetectionHandler`), and `OnTrajectoryComplete` provides the trace of an electron's path (handled by `TMainForm.TrajectoryCompleteHandler`). This mechanism is crucial for displaying progress and results in real-time.

## Project Structure and Units

The project is organized into several Pascal units (`.pas` files):

*   `et_global.pas`: Defines global data types, constants, and variables. (Detailed above)
*   `et_math.pas`: Provides mathematical functions and vector/ray operations. (Will be detailed in a later section)
*   `et_objects.pas`: Defines the core simulation objects and material properties. (Detailed above)
*   `et_params.pas`: Contains functions for reading simulation parameters from a configuration file (`etracer.cfg` or `CALC_ET.CFG`) and some related global variables. (Will be detailed in a later section)
*   `et_sim.pas`: Implements the main simulation logic and the `TSimulation` class. (Detailed above)
*   `et_main.pas`: Contains the GUI definition (`TMainForm`) and handles user interaction, parameter loading/saving, running simulations, and displaying results via charts and memos. It acts as the orchestrator using the classes from other units. (Will be detailed in a later section)
*   `unit_tests/etmathtests.pas`: Unit tests for `et_math.pas`.
*   `unit_tests/etsampletests.pas`: Unit tests for the `TSample` implementations in `et_objects.pas`.

This documentation will further detail each major component and unit.

## Building the Project

The ETrace project is written in Free Pascal and can be compiled using the Free Pascal Compiler (`fpc`). The main application with the GUI is defined in `et_main.pas`.

### Prerequisites

*   **Free Pascal Compiler (FPC)**: You need `fpc` installed on your system. You can download it from the official Free Pascal website (https://www.freepascal.org/) or install it via your operating system's package manager (e.g., `sudo apt-get install fpc` on Ubuntu, `brew install fpc` on macOS via Homebrew, install from package managers on other systems).
*   **Lazarus Component Library (LCL)**: The GUI (`et_main.pas`) uses the LCL, which is Free Pascal's equivalent to Delphi's VCL. This is typically included with a standard FPC installation or when installing the Lazarus IDE. Ensure your FPC installation includes LCL support for the relevant widgetset for your operating system (e.g., GTK2/QT for Linux, WinAPI for Windows, Cocoa for macOS).
*   **TAChart Package**: The project uses the TAChart library for plotting and charting functionalities. This is a widely used charting package for Free Pascal and Lazarus. It is usually available through Lazarus IDE installation or can be installed separately for FPC. Ensure the TAChart source files and compiled units are accessible to the compiler. When installing FPC or Lazarus via package managers or official installers, TAChart is often an optional component you need to explicitly select. If compiling from the command line, you may need to add the TAChart unit path using an `-Fu` flag during compilation.

### Building with Lazarus IDE

While command-line compilation is feasible, Free Pascal projects with GUIs built using LCL are typically developed and managed using the Lazarus IDE. Lazarus simplifies the build process significantly by providing a visual form designer, integrated debugger, and automated handling of unit paths and required packages (like LCL and TAChart) linked to the project.

1.  **Open Project**: If a Lazarus project file (`.lpr` for the main program, `.lpi` for the project information) exists for ETrace (these were not provided in the interfaces, but are standard for Lazarus projects), open it using the Lazarus IDE's "Project" -> "Open Project..." menu option.
2.  **Configure Project**: If no project file exists, you might need to create a new "Application" project in Lazarus and add the existing `.pas` files (`et_main.pas`, and potentially adding the `src` and `unit_tests` directories to the project's search paths in the project options). Ensure the required packages (LCL, TAChart) are added to the project dependencies (Project -> Project Options -> Packages).
3.  **Build**: Use the "Run" -> "Build All" or "Run" -> "Compile" menu option in Lazarus. The IDE will invoke the FPC compiler with all the necessary options derived from the project configuration.
4.  **Run**: Use the "Run" -> "Run" option (usually F9) to compile the project (if necessary) and launch the application directly from within the IDE. This is convenient for testing and debugging.

Using Lazarus is generally the recommended approach for building and developing LCL-based applications like ETrace, as it automates many configuration details required for the GUI components and external libraries.

By following these steps, you should be able to build the ETrace application from the source code using the Free Pascal Compiler, either via the command line or the Lazarus IDE.

### Mathematical Utilities (`et_math.pas`)

This unit provides a collection of mathematical constants, functions, and vector/ray operations necessary for the 3D simulations.

*   **Constants**: Defines common mathematical constants like `FloatEps`, `Pi_2`, `TwoPi`, `FourPi`, `OneThird`, `TwoThirds`. `FloatEps` is likely used for floating-point comparisons with a tolerance.
*   **Comparison Functions**: Functions for comparing floating-point numbers with a tolerance (`Tol`):
    *   `Between(x, lo, hi, Tol)`: Checks if `x` is strictly between `lo` and `hi`.
    *   `BetweenIncl(x, lo, hi, Tol)`: Checks if `x` is between `lo` and `hi` inclusive.
    *   `Equal(x, y, Tol)`: Checks if `x` is approximately equal to `y`.
    *   `GreaterEqual(x, y, tol)`, `GreaterThan(x, y, tol)`: Checks for greater than/equal or greater than relationships with tolerance.
    *   `LessEqual(x, y, tol)`, `LessThan(x, y, tol)`: Checks for less than/equal or less than relationships with tolerance.
    *   `Zero(x, Tol)`: Checks if `x` is approximately zero.
*   **Random Number Functions**:
    *   `Random_Gauss`: Returns a random number from a Gaussian (normal) distribution, likely used for simulating beam profile or scattering angles.
    *   `Random_Cos`: Returns a random number distributed according to a cosine function, possibly used for simulating directional distributions (e.g., of emitted electrons).
*   **Quadratic Solver**:
    *   `SqrSolve(p, q, var x1, x2)`: Solves the reduced quadratic equation `x^2 + p*x + q = 0`.
    *   `SqrSolve(a, b, c, var x1, x2)`: Solves the standard quadratic equation `a*x^2 + b*x + c = 0`. These are essential for calculating intersection points with quadratic surfaces like cylinders.
*   **Utility Procedures**:
    *   `SwapFloat(var x, y)`: Swaps the values of two float variables.
*   **Vector3 Functions and Operators**: These operate on the `TVector3` record type.
    *   `Vector3(x, y, z)`: Creates a `TVector3` from components.
    *   `EmptyVector3`: Returns a zero vector (0, 0, 0).
    *   `ValidVector(V)`: Checks if a vector contains valid (not NaN or Infinity) float values, useful for checking calculation results.
    *   `DotProduct(const A, B)`: Calculates the dot product of two vectors (`A.X*B.X + A.Y*B.Y + A.Z*B.Z`), used for angle calculations and projections.
    *   `CrossProduct(const A, B)`: Calculates the cross product of two vectors, yielding a vector perpendicular to both A and B. Useful for finding surface normals or rotation axes.
    *   `VecLength(const A)`: Calculates the magnitude (length) of a vector (`sqrt(A.X^2 + A.Y^2 + A.Z^2)`).
    *   `VecNormalize(const A)` / `VecNormalize(X, Y, Z)`: Returns a normalized version of a vector (a unit vector with length 1) by dividing by its length. Crucial for representing directions.
    *   `VecAngle(const A, B)`: Calculates the angle between two vectors using the dot product and arc cosine.
    *   `VecRotate(const V, A, phi)`: Rotates vector `V` around axis `A` by angle `phi`.
    *   `Rotate(var V: TVector3; A: TVector3; Cos_phi,Sin_phi: Float)`: Rotates vector `V` around axis `A` using pre-calculated cosine and sine of the angle `phi`. This is a performance optimization.
    *   `VecRotateY(const V, phi)`: Rotates vector `V` specifically around the Y-axis by angle `phi`.
    *   `Distance(const A, B)`: Calculates the Euclidean distance between two points represented by vectors (`VecLength(A - B)`).
    *   Operators `+`, `-`, `*`: Overloaded standard operators for vector addition (`A + B`), subtraction (`A - B`), negation (`-A`), and scalar multiplication (`A * x` or `x * A`). These allow for intuitive vector arithmetic.
*   **Ray-Object Intersection Functions**:
    *   `rayXplane(ray: TRay; Plane: TRay; var Point: TVector3)`: Calculates the intersection point of a ray with an infinite plane. The plane is defined by a point on the plane (`Plane.Point`) and its normal vector (`Plane.Dir`). Returns the parameter `t` such that `ray.Point + t * ray.Dir` is the intersection point. If no intersection or the ray is parallel to the plane, it likely returns a specific value (e.g., a very large number or -1).
    *   `rayXcyl(ray: TRay; r: float; var Point:TVector3; FarPoint: boolean)`: Calculates the intersection point of a ray with a cylinder of radius `r`. The axis of the cylinder is implicitly the Z-axis in this context (as often seen in surface analysis geometries). `FarPoint` likely determines whether the function returns the intersection point closest to the ray's origin (`false`) or the farther one (`true`). Returns the parameter `t` along the ray.
*   **Coordinate Transformation Functions**:
    *   `CartToSph(x, y, z, var theta, phi)`: Converts Cartesian coordinates (x, y, z) to standard spherical coordinates (theta - polar angle, typically from Z-axis; phi - azimuthal angle, typically in XY plane from X-axis).
    *   `SphToCart(theta, phi, var x, y, z)` / `SphToCart(Cos_theta, Sin_theta, Cos_phi, Sin_phi, var x, y, z)`: Converts spherical coordinates back to Cartesian coordinates. The second version using pre-calculated sines/cosines is a performance optimization.
    *   `CartToCyl(x, y, var rho, phi)`: Converts Cartesian coordinates (x, y) in the XY plane to polar coordinates (rho - radial distance from origin, phi - azimuthal angle).
    *   `CylToCart(rho, phi, var x, y)`: Converts polar coordinates (rho, phi) in the XY plane back to Cartesian coordinates (x, y).

This unit provides the comprehensive set of geometric and numerical capabilities required by the simulation engine and sample objects to perform 3D tracing and interaction calculations.

### Parameter Handling (`et_params.pas`)

This unit focuses on managing simulation parameters, particularly loading them from a configuration file.

*   **`CfgName`**: A global string constant defining a default configuration file name: `'CALC_ET.CFG'`. Note that `et_global.pas` defines `CFG_FILE_NAME = 'etracer.cfg'`. Based on the `et_main.pas` interface, `etracer.cfg` appears to be the primary configuration file used by the GUI (`TMainForm`) for loading and saving parameters via `LoadParamsFromCfg`/`SaveParamsFromCfg` and the `TIniFile` component. The exact role of `CALC_ET.CFG` is less clear from the provided interfaces alone and might be used in a different context (e.g., an older command-line version or internal process) or might be a remnant. It is recommended to clarify this if full source code is available.
*   **Global Variables**: This unit declares several global variables, likely intended to hold parameters and simulation state, potentially loaded from the configuration file or updated during execution. Some overlap conceptually with fields in `TSimParams` or `TMainForm`, suggesting different purposes or historical use.
    *   `MaxEl`: Integer, likely the maximum number of electrons to simulate (default 100). This might be related to `TSimParams.NumElectrons` or `TSimulation.FMaxPrimElectrons`.
    *   `TraceName`, `EmPtFName`: Strings, possibly file names for output files where trajectories are saved (`TraceName`) and where emission points are saved (`EmPtFName`).
    *   `TopInt`, `WallInt`, `BotInt`: Floats, potentially storing integrated Auger electron intensity values for different defined areas of the sample surface (e.g., the top flat surface, side walls of a hole/stripe, the bottom of a hole). These might be updated during simulation and displayed in the GUI summary.
    *   `EmPoints`: Declared as `TMatrix`, but the definition of `TMatrix` is not present in the provided interfaces. Based on its name and context with `EmPtFName` and the `FEmissionPoints` array in `TMainForm`, it is likely intended to be a data structure (possibly from an external graphics or matrix library, or an internal internal array type not fully described) to store the coordinates of detected Auger electron emission points. This variable might be related to or potentially superseded by the `FEmissionPoints: array of TVector3` used in `TMainForm` for charting.
    *   `nDet`, `nDetOld`: Integers, counters for detected Auger electrons. `nDet` is likely the current count during a simulation run, and `nDetOld` might represent a count from a previously loaded emission points file (`EmPtFName`).
    *   `nPrim`: Integer, counter for the number of primary electrons simulated in the current run. This is likely linked to `TSimulation.FNumPrimElectrons`.
    *   `StartTime`: `TDateTime`, records the simulation start time, used for calculating simulation duration.
    *   (Commented-out variables like `SetStart`, `Graphik`, `HorRange`, `VertRange`, `Projection`, `PlotColor`, `GrBkColor`, `GrColor`, `BeepOn` suggest features that were removed, are part of an alternative command-line interface, or were intended for different plotting capabilities than the current `TAChart` implementation in `et_main.pas`).
*   **`CreateIni(ACfgFile: String)`**: Function that likely creates and returns a `TCustomIniFile` object (or a descendant like `TIniFile`) initialized to work with the specified configuration file name (`ACfgFile`). This object provides standard methods (like `ReadString`, `WriteInteger`, etc.) to interact with data stored in the INI file format (`[Section] Key=Value`).
*   **`ReadParamsFromCfg(ACfgFile: String)`**: Procedure that reads simulation parameters from the specified configuration file (`ACfgFile`) using an INI file reader (`TCustomIniFile`) and populates some of the global variables declared in `et_params.pas` (e.g., `MaxEl`, `TraceName`, `EmPtFName`). Note that the `et_main.pas` unit has its own `LoadParamsFromCfg` and `SaveParamsFromCfg` procedures which directly interact with the `TSimParams` record and GUI controls, suggesting the primary parameter loading/saving logic for the GUI is handled there, possibly using `et_params.pas` helper functions or the `IniFiles` unit directly.

While this unit provides functions for basic INI file handling and defines some global state variables, the main GUI's comprehensive parameter management appears to be primarily centered around the `TSimParams` record in `et_global.pas` and the dedicated `LoadParamsFromCfg`/`SaveParamsFromCfg` methods within `et_main.pas`.

### Main Application Form (`et_main.pas`)

This unit contains the definition of the main application window, `TMainForm`, which serves as the graphical user interface (GUI) for the ETrace project. It integrates the simulation logic from other units and provides controls for setting parameters, running simulations, and visualizing results.

*   **`TMainForm`**: The main form class, inheriting from `TForm` (part of LCL). It contains various visual components (from LCL and TAChart libraries) and methods to manage user interaction and simulation execution.
    *   **Visual Components**: The interface lists numerous components (`TButton`, `TCheckBox`, `TComboBox`, `TFloatSpinEdit`, `TSpinEdit`, `TPanel`, `TGroupBox`, `TLabel`, `TRadioGroup`, `TProgressBar`, `TMemo`, `TPageControl`, `TSpeedButton`, `TChart`, `TLineSeries`, `TUserDefinedChartSource`). These components form the layout of the main window and allow the user to input values for the `TSimParams` record, initiate actions, monitor simulation progress, and view visual outputs like charts and text logs.
    *   **Event Handlers**: Numerous procedures associated with specific component events (e.g., `btnRunSimClick` for button clicks, `cmbAnalyzerTypeChange` for combo box selection changes, `FormCreate` for form initialization, `rgProjectionClick` for radio group selection). These methods contain the logic that is executed in response to user interactions or form lifecycle events.
    *   **Private Fields**: These fields store data managed by the form, often related to the simulation state and results being prepared for display.
        *   `FEmissionPoints`: An array of `TVector3`. This dynamic array stores the calculated 3D coordinates on the sample surface where detected Auger electrons originated. This data is used to populate the `EmissionPointsChart` via the `EmissionPointsSource`.
        *   `FTrajectories`: An array of `TTrajectory` (where `TTrajectory` is an array of `TTrajectoryPoint` defined in `et_sim.pas`). This array stores the sequences of points representing the paths taken by simulated electrons, used for visualization on the `TrajectoriesChart`.
        *   `FSampleHitPoint`: `TVector3`. Stores the initial impact point of the primary electron beam on the sample surface, potentially for display or reference.
        *   `FRunning`, `FAborted`: Boolean flags indicating the current simulation state (`FRunning` is true if a simulation is active, `FAborted` is true if the user requested cancellation). Used to control GUI element state (`UpdateCtrlState`) and simulation flow (`CancelHandler`).
        *   `FActivated`: Boolean flag, likely used to track whether the form's `OnActivate` event has fired, which can be useful for initial setup routines that should only run once the form is fully displayed.
    *   **Private Procedures**: These methods encapsulate the internal logic of the form, including data transfer, setup, execution control, and result display.
        *   `DisplaySummary(ASimulation)`: Takes a `TSimulation` object and formats key results (like detection counts, simulation parameters, timing) into a human-readable string to be displayed in the `SummaryMemo`.
        *   `GetProjection`: Reads the state of the `rgProjection` radio group and returns the corresponding `TProjection` enumeration value (e.g., `XYproj`, `ThreeD`) to control how trajectory data is displayed.
        *   `GetSelectedAnalyzer`, `GetSelectedTopography`: Reads the selected item in the respective `TComboBox` controls and returns the corresponding enumeration value (`TAnalyzerType`, `TTopoType`).
        *   `GUIToParams(var AParams)`: Populates a `TSimParams` record with the values currently set in the various GUI controls (spin edits, combo boxes, checkboxes, radio group). This function is called before starting a simulation to gather all user-defined parameters.
        *   `InitMaterial(AMaterials, AName)`: A helper procedure used during initialization to find or create a `TMaterial` object based on a material name from a provided list (`TMaterialsList`). This is likely used to set up the `Substrate` and `Layer` materials for the sample.
        *   `LoadParamsFromCfg`, `SaveParamsFromCfg`: Procedures to handle loading and saving the entire set of simulation parameters (likely stored in a `TSimParams` record) from/to the `etracer.cfg` configuration file using the `TIniFile` component.
        *   `ParamsToGUI(const AParams)`: The inverse of `GUIToParams`. It takes a `TSimParams` record (e.g., loaded from a config file) and updates all the relevant GUI controls to reflect these parameter values.
        *   `PopulateMaterialsCombo(ACombobox)`: Fills a given `TCombobox` component (used for selecting substrate and layer materials) with the names of available materials by querying the `TMaterialsList`.
        *   `PrepareSim`: Performs necessary setup steps before a simulation run is initiated. This might include clearing previous results, initializing variables, and creating the `TSimulation` object but not starting its execution.
        *   `RunSimulation`: This is the core procedure triggered by the "Run Simulation" button. It calls `PrepareSim`, gathers parameters from the GUI using `GUIToParams`, creates and configures the `TSimulation` instance, sets up the event handlers (`OnCancel`, `OnDetection`, `OnTrajectoryComplete`) to point to methods within `TMainForm`, and finally calls `TSimulation.Execute`, managing the process to allow the GUI to remain responsive.
        *   `UpdateCtrlState(AEnabled)`: A utility procedure to enable or disable groups of GUI controls, preventing the user from changing parameters or starting a new simulation while one is already running.
        *   `ReadIni`, `WriteIni`: Helper procedures, possibly separate from the main simulation parameter handling, used for loading/saving general application settings (e.g., window size, position, last used directory) using INI files.
        *   **Event Handlers for `TSimulation`**: These methods are callback procedures assigned to the event properties (`OnCancel`, `OnDetection`, `OnTrajectoryComplete`) of the `TSimulation` object. The simulation engine calls these methods during its execution to report status and results back to the GUI thread.
            *   `CancelHandler(ASimulation, var Cancel)`: This method is called periodically by the simulation engine to check if it should stop. It sets the `Cancel` parameter to `True` if the user has somehow signaled cancellation via the GUI (e.g., by clicking a "Cancel" button, implicitly setting the `FAborted` flag).
            *   `DetectionHandler(ASimulation, AElectronCount, const Electron)`: Called by `TSimulation` whenever an Auger electron is successfully detected by the simulated analyzer. It receives the total count of detected electrons so far and the details of the newly detected `TAugerElectron`. This method is responsible for updating detection counters (`nDet`) and storing the electron's emission point (`Electron.Ray.Point`) in the `FEmissionPoints` array.
            *   `TrajectoryCompleteHandler(Simulation, const AElectronID, const ATrajectory)`: Called by `TSimulation` after tracing the complete path of a primary or secondary electron. It receives a unique identifier for the electron and the `TTrajectory` data (an array of `TTrajectoryPoint`). This method stores the completed trajectory data in the `FTrajectories` array for subsequent visualization on the `TrajectoriesChart`.

In summary, `et_main.pas` is the primary interface unit. It provides the visual controls for setting parameters, implements the logic to initiate and monitor simulations using the `TSimulation` class, and handles the display of results via charts and text memos, utilizing event handlers to receive updates from the simulation engine.

## Unit Tests

The project includes unit tests to verify the correctness of key functionalities, particularly in the mathematical calculations and sample geometry intersection logic. These tests are written using the Free Pascal Unit (FPCUnit) testing framework. Running these tests helps ensure the core physics and geometry calculations are performed correctly.

*   **`unit_tests/etmathtests.pas`**: This unit contains test cases specifically designed to verify the mathematical functions and vector operations defined in the `et_math.pas` unit.
    *   `TMathTests = class(TTestCase)`: This is the test case class that groups related math tests. It inherits from `TTestCase` provided by FPCUnit.
    *   `procedure TestVectorMath;`: This published method likely tests the vector arithmetic operators (`+`, `-`, `*`) and key functions like `DotProduct`, `CrossProduct`, `VecLength`, `VecNormalize`, and `VecAngle` with various input vectors to ensure they produce the correct results.
    *   `procedure TestIntersection_RayPlane;`: This published method tests the `rayXplane` function from `et_math.pas`. It likely sets up various scenarios with different ray origins, directions, and plane definitions (via `Plane: TRay`) and verifies that the calculated intersection point (`Point: TVector3`) and distance (`Float` return value) are correct for intersecting, parallel, and non-intersecting cases.
*   **`unit_tests/etsampletests.pas`**: This unit contains test cases focused on verifying the intersection calculations for the concrete sample geometry classes defined in `et_objects.pas`.
    *   `TSampleTests = class(TTestCase)`: The test case class for sample geometry intersection tests, inheriting from `TTestCase`.
    *   `protected FSubstrate: TMaterial; FLayer: TMaterial;`: These protected fields likely store material objects (`TMaterial`) that are used in the tests to create sample instances.
    *   `procedure Setup; override;`: This procedure is automatically called by the FPCUnit test runner before each test method in the class is executed. It is used here to perform common initialization, such as creating and configuring the `FSubstrate` and `FLayer` material objects using parameters from `TMaterialParams` or similar.
    *   `procedure TearDown; override; override;`: This procedure is automatically called by the FPCUnit test runner after each test method completes. It's used for cleanup, such as freeing the `FSubstrate` and `FLayer` objects created in `Setup`.
    *   `procedure TestIntersection_ContactHole;`: This published method tests the `Intersection` method of the `TContactHole` class. It involves creating `TContactHole` instances with specific dimensions, setting up various rays originating from inside or outside the hole geometry, and verifying that the `Intersection` method correctly identifies whether an intersection occurs and calculates the accurate 3D intersection point (`Point: TVector3`) and distance along the ray.
    *   `procedure TestIntersection_Stripe;`: This published method tests the `Intersection` method of the `TStripe` class similarly to the `ContactHole` test. It verifies that the method correctly handles ray intersections with the rectangular stripe geometry for different ray paths and origins.

These unit tests are crucial for validating the correctness of the core geometric and mathematical routines that underpin the simulation. Running these tests using the FPCUnit test runner (a separate executable typically built from the `fpcunit.lpr` project or a simple test runner program) is highly recommended after any changes to the `et_math.pas` or `et_objects.pas` units to ensure that functionality remains correct and to catch bugs early.

## Conclusion and Further Steps

ETrace offers a robust and modular framework for simulating complex electron scattering and Auger emission processes in materials with various topographies, making it a valuable tool for research, development, and education in the field of surface analysis and electron microscopy. The separation of the core simulation logic from the GUI allows for potential extensions and adaptations, such as developing a command-line interface or integrating the simulation engine into other software.

**To explore further and contribute:**

*   Examine the full source code implementation (`.pas` files beyond the provided interfaces) to gain a deeper understanding of the detailed algorithms and logic within the methods and procedures described, particularly the implementation details of `TSimulation.CalcTrajectory` and the specific physics models used for scattering and stopping power in the `TMaterial` class.
*   Compile the project using the instructions provided in the "Building the Project" section. Start with command-line compilation to understand the process, and consider using the Lazarus IDE for development and easier management of dependencies and the GUI.
*   Run the application and experiment with different simulation parameters and sample geometries via the GUI to observe the effects on electron trajectories and signal generation in practice. Refer to the "How to Use ETrace" section.
*   Run the unit tests (`unit_tests/etmathtests.pas`, `unit_tests/etsampletests.pas`) using the FPCUnit test runner to verify the integrity of the core mathematical and geometric calculations. This is a crucial step before making modifications.
*   Study the relationship and data flow between the `TSimParams` record (defining simulation inputs), the GUI controls in `et_main.pas` (gathering user input), the configuration file (`etracer.cfg`), and how these parameters are used to instantiate and configure the `TSimulation`, `TElectronSource`, `TSample`, and `TAnalyzer` objects.
*   Consider contributing to the project by adding new features, such as implementing additional sample geometries (requiring a new class inheriting from `TSample` and implementing its abstract methods), incorporating more advanced physics models for electron-solid interactions, implementing different types of electron analyzers, enhancing the user interface, adding new visualization options, or developing a command-line interface. The existing structure provides a solid foundation for such extensions.

ETrace stands as a powerful and flexible tool for researchers and developers in electron spectroscopy and microscopy. We hope this documentation serves as a comprehensive and helpful guide for building, understanding the inner workings of, using, and potentially contributing to the project.
