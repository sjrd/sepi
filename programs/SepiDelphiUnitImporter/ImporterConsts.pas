unit ImporterConsts;

interface

resourcestring
  // Context errors
  SBDSVersionNotInstalled = 'La version %s de BDS n''est pas installée';
  SBDSVendorVersionNotInstalled =
    'La version %1:s de BDS de %0:s n''est pas installée';

  // Options errors
  SOneOrMoreFileNamesRequired = 'Un argument ou plus requis';

  // Importer errors
  SCantFindProgram = 'Ne peut trouver le programme externe %s';
  SCantFindSourceFile = 'Le fichier source %s n''existe pas';
  SCantOpenSourceFile = 'Impossible de créer le fichier source %s';
  SCantOpenDestFile = 'Impossible de créer le fichier de sortie %s';
  SSepiInternalError = 'Erreur interne : %s';
  SCantCompileRCFile = 'Ne peut compiler le fichier de ressources %s';

const // don't localize
  PascalExt = '.pas';
  CompiledIntfExt = '.sci';
  RCExt = '.rc';
  MaxSizeBeforeLazyLoad = 100*1024;

  DefaultTemplatesDir = 'Templates\';
  DefaultCacheDir = 'Cache\';
  DefaultOutputDir = 'Output\';
  DefaultResourcesDir = 'Resources\';

implementation

end.

