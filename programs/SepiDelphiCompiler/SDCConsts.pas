{-------------------------------------------------------------------------------
SepiDelphiCompiler - Example program for Sepi
As an example program, SepiDelphiCompiler is free of any usage. It is released
in the public domain.
-------------------------------------------------------------------------------}


{*
  Constantes du projet SepiDelphiCompiler
  @author sjrd
  @version 1.0
*}
unit SDCConsts;

interface

resourcestring
  // Compiler errors
  SCantFindSourceFile = 'Le fichier source %s n''existe pas';
  SCantOpenSourceFile = 'Impossible de créer le fichier source %s';
  SCantOpenDestFile = 'Impossible de créer le fichier de sortie %s';
  SSepiInternalError = 'Erreur interne : %s';
  SCantCompileRCFile = 'Ne peut compiler le fichier de ressources %s';

  // Options errors
  SOneOrMoreFileNamesRequired = 'Un argument ou plus requis';

const // don't localize
  DefaultOutputDir = 'Output\';

const // don't localize
  PascalExt = '.pas';
  CompiledIntfExt = '.sci';
  CompiledUnitExt = '.scu';
  RCExt = '.rc';
  TextExt = '.txt';
  MaxSizeBeforeLazyLoad = 100*1024;

implementation

end.

