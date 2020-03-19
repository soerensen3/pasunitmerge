# pasunitmerge

## Purpose

pasunitmerge is a utility for Lazarus to merge multiple units to one unit. Why would one merge multiple units to one? The reason is to achieve the opposite! You can have a lot of very small units separated from each other that however depend on each other using forward declarations accross units. 

This utility is functional while it certainly has a lot of bugs. It's merely an experiment because I want to find a way to deal with unseparable units. I use includes for it a lot but I am not totally happy with it.

## Example

Sorry, the example is not very good for now because you could do it without forward declaration.


    unit MyUnit.TestClass;
    
    interface 
    
    uses SysUtils, Classes;
    
    type
      TTestBaseClass = class; //forward
      TTestClass = class ( TTestBaseClass )
        
      end;
         
    ...




    unit MyUnit.TestBaseClass;
    
    interface 
    
    uses Classes, fgl;
    
    type
      TTestBaseClass = class
        
      end;
         
    ...
    
    
    unit MyUnit.TestClass;
    
    interface 
    
    uses SysUtils, Classes, fgl;
    
    type
      TTestBaseClass = class; //forward
      TTestClass = class ( TTestBaseClass )
        
      end;

      TTestBaseClass = class
        
      end;
         
    ...

       
    
## Usage:

call 

    pasunitmerge MyUnit.Testclass.pas MyUnit.TestBaseClass.pas -o MyUnit.pas [More Units -o Outfile]
