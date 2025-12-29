# DI2ABAP Framework
A modern dependency injection framework for SAP ABAP

## An ABAP Framework for Dependency Injection
DI2ABAP is a lightweight, annotation-based dependency injection framework (imperative approach is also supported) that brings all modern development patterns into the SAP ecosystem. Inspired by the Java Spring Framework and created specifically with ABAP limitations in mind, it ensures a cleaner architecture, better testability, and reduced coupling in applications.

## Project Structure
```dotenv
|project/
|--- zdi2abap/             - Core framework classes
|   |--- CLASSES/          - Classes
|   |--- INTERFACES/       - Interfaces
|   |--- MSGCLASS/         - Message classes
|--- zdi2abap_documentation/ - Auto-documentation report for dependencies
|   |--- CLASSES/          - Classes
|   |--- PROGRAMS/         - Reports
|   |--- INTERFACES/       - Interfaces
|   |--- INCLUDES/         - Includes
|   |--- TEMPLATES/        - Templates for upload to SMW0
|--- zdi2abap_examples/    - Examples
|--- README.md
|--- Annotations.md
```

## Key Features
- Annotation-based dependency injection (@Component, @Inject, @Qualifier)
- Singleton and Prototype scopes
- Constructor dependency injection via the @Inject annotation on attributes
- Circular dependency detection
- Component lifecycle management
- Ability to proxy any component-related interfaces and intercept before/after calls (for profiling, metrics, logging)
- Imperative approach to dependency management without direct use of annotations, via configuration extension classes. Possibility to combine imperative and declarative approaches.
- Dependency graph construction with output to a report using Vis.js. Export to JSON/PNG.

![complex.png](complex.png)

## Design

```dotenv
┌─────────────────────────────────────────────────┐
│              ZCL_DI2ABAP                        │
│                (Facade)                         │
└───────────────┬─────────────────────────────────┘
                │
    ┌───────────┼───────────┐
    ▼           ▼           ▼
┌─────────┐ ┌───────┐ ┌────────┐
│Container│ │Scanner│ │Resolver│
└────┬────┘ └───────┘ └────────┘
     │
     ▼
┌─────────────────────────────────────────────────┐
│         Application Components                  │
│  ┌──────────┐   ┌──────────┐   ┌──────────┐     │
│  │ Services │   │Repository│   │ Others.. │     │
│  └──────────┘   └──────────┘   └──────────┘     │
└─────────────────────────────────────────────────┘
```

Container - ZCL_DI_CONTAINER. The class responsible for storing instances of all components.
Scanner - ZCL_DI_SCANNER. Scans classes in specified packages. Parses annotations, forms a list of dependencies for injection.
Resolver - A class for resolving dependency conflicts. Forms the correct sequence for instantiating dependencies using topological sorting.

## How to Use
1. Create a transport request
2. Create objects in the SAP system
3. Copy code from the corresponding files into objects with the same names
4. Activate the objects
5. See examples in the ZDI2ABAP_EXAMPLES package

## Dependencies
- Requires SAP_BASIS 740+

## Quick Start

1. Define your components

```abap
"@Repository
CLASS zcl_user_repository DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_user_repository.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

"@Service
CLASS zcl_user_service DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_user_service .

    METHODS constructor
      IMPORTING
        io_repository TYPE REF TO object.
  PROTECTED SECTION.

    "@Inject( io_repository )
    DATA mo_repository TYPE REF TO zif_user_repository.
  PRIVATE SECTION.
ENDCLASS.
```


2. Get components from the container

```abap
DATA:
  lo_service   TYPE REF TO zcl_user_service,
  lo_repository TYPE REF TO zif_user_repository.

DATA(lo_container) = zcl_di2abap=>get_instance( )->create_container(
                                                           VALUE #( ( `ZYOUPACKAGE` ) )
                                                         ).
lo_container->resolve( IMPORTING eo_instance = lo_service ).
lo_container->resolve( IMPORTING eo_instance = lo_repository ).
```

## Benefits of Using the Framework

- Less boilerplate code
- Automatic dependency resolution, standardized dependency management
- Testability with easy mocking
- Clean separation of concerns

## Contacts
- Developer: Denisov Nikolay
- email: trash789012@gmail.com
- tg: @denials45