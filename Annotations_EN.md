# This file contains a list of used annotations along with brief comments

## Component Annotations

- @Core - The framework core. Does not provide any benefits when used in other classes.

- @Component - Declares a class for use.
  It will become part of the graph (a component) and participate in topological sorting.
  The framework will create its instance when requested via resolve,
  or if any component requires its injection.

- @Repository - This is a @Component. The difference is semantic.
  By design, these are classes that perform any database operations.

- @Service - This is a @Component. The difference is semantic.
  By design, these are classes that contain business logic (and often use repositories for CRUD).

- @Interceptor - This is a @Component. Additionally, the class is marked
  as an interceptor for before/after methods of proxy objects. An interceptor class must implement the interface
  zif_di_proxy_interceptor and be unique within the scanned packages.

- @Configuration - Not a component. This annotation is needed so that the class is picked up by the scanner.
  A configuration class must implement the interface zif_di_app_config_enhancer.
  Upon discovery, the framework will call the method zif_di_app_config_enhancer~configurate,
  passing the configurator object (zif_di_app_config). After this, the lifecycle of the @Configuration class ends,
  and its instance is destroyed.
  There can be any number of configurator classes within the scanned packages,
  but their settings may conflict and the order of execution is not guaranteed.

Hierarchically, component annotations can be represented as follows:

```dotenv
|- @Configuration
|- @Component
|--- @Core
|--- @Repository
|--- @Service
|--- @Interceptor
```

## Scope Annotations that define component lifecycle

- @Scope( Singleton ) - One instance per application. Not recreated. The annotation can be omitted, as by default the scope for any component is Singleton.
- @Scope( Prototype ) - A new instance for each object request. Every time an object is required for injection, a new instance is created. If the object also contains dependencies with @Scope( Prototype ), they will also be created anew.

## Component Qualifier

If there are several components of the same type in the application, which often happens, you can and should specify a qualifier,
which will allow unambiguous identification of the component.

- @Qualifier( QualifierValue )

Qualifier is used only for @Component. For example:

```abap
@Component
@Qualifier( HttpService )
CLASS zcl_service DEFINITION.
ENDCLASS.
```

## Marking for Dependency Injection

Class attributes, regardless of access modifier, can be injected automatically into the class via the class constructor.
For this, the class attribute must be marked with the annotation:

- @Inject( <constructor_parameter_name> )

And the class constructor must have a parameter named <constructor_parameter_name>:

```abap
"@Component
CLASS zcl_demo_inject DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          io_service TYPE REF TO service.
  PROTECTED SECTION.
    "@Inject( io_service )
    DATA mo_service TYPE REF TO zif_service.
ENDCLASS.
```

However, specifying the value <constructor_parameter_name> in the @Inject annotation itself is optional.
If not specified, the framework will expect the constructor parameter to have the same name as the attribute.

```abap
"@Component
CLASS zcl_demo_inject DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          mo_service TYPE REF TO service.
  PROTECTED SECTION.
    "@Inject
    DATA mo_service TYPE REF TO zif_service.
ENDCLASS.
```

## Dependency Qualifier

The dependency qualifier allows specifying the exact implementation to inject.
Used in combination with the @Inject annotation.

- @RefQualifier( QualifierValue )

For the operation to succeed, a @Component with the qualifier QualifierValue must exist in the container,
i.e., @Qualifier( QualifierValue ).

Example:

```abap
@Component
@Qualifier( HttpService )
CLASS zcl_service DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_service.
ENDCLASS.

CLASS zcl_demo_inject DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          io_service TYPE REF TO service.
  PROTECTED SECTION.
    "@Inject( io_service )
    "@RefQualifier( HttpService )
    DATA mo_service TYPE REF TO zif_service.
ENDCLASS.
```

## Working with an Object via Proxy Class (Interceptor)

This works only with classes that implement at least one interface.
Every component that implements an interface can be proxied.

- @Proxy( true ) | @Proxy( false )

For method interception, the framework uses transient generation (GENERATE SUBROUTINE POOL)
of the class zcl_aop_base.
A proxy class with the same interface is generated, and the original object is passed to the proxy class.
During dependency assembly, the container sees that the class needs to be proxied and returns, instead of the original, the interface object
created during generation.
Any method call on the proxied object transfers control to the class marked as @Interceptor,
which implements the interface zif_di_proxy_interceptor.
The client believes it is working with the original, as the interface remains unchanged.
In practice, this allows for logging, memory snapshots, and more.