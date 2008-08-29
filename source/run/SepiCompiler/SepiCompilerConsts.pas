unit SepiCompilerConsts;

interface

resourcestring
  // Format des erreurs
  SSepiHintName = 'Conseil';
  SSepiWarningName = 'Avertissement';
  SSepiErrorName = 'Erreur';
  SSepiFatalErrorName = 'Erreur fatale';
  SSepiCompilerErrorFormat = '[%s] %s(%d) : %4:s';

  // Erreurs de compilation génériques
  SSepiThereWereErrors = 'Il y a eu des erreurs à la compilation';
  SIdentifier = 'Identificateur';
  SRedeclaredIdentifier = 'Identificateur redéclaré';
  SIdentifierNotFound = 'Identificateur %s non déclaré';
  STypeMismatch = 'Types incompatibles : %s attendu mais %s trouvé';
  SNeedConstExpression = 'Expression constante attendue';

  // Erreurs de compilateur
  SLabelAlreadyExists = 'Le label ''%s'' existe déjà';
  SLabelNotFound = 'Label ''%s'' non trouvé';
  SMemoryRefIsSealed = 'La référence mémoire est scellée';
  SMemoryCantBeZero = 'La référence mémoire ne peut être zéro';
  SMemoryCantBeConstant = 'La référence mémoire ne peut être constante';
  SMemoryCantBeTrueConst =
    'La référence mémoire ne peut être une vraie constante';
  SMemorySpaceOffsetMustBeWord =
    'L''offset d''espace mémoire doit être contenu dans un Word';
  SMemoryCantAccessObject = 'Impossible d''accéder à l''objet %s';
  STooManyOperations =
    'Une référence mémoire ne peut avoir que 15 opérations maximum';
  SZeroMemoryCantHaveOperations =
    'Une référence mémoire vers des 0 ne peut avoir d''opérations';
  SConstArgMustBeShort =
    'L''argument constant doit être contenu dans un Shortint';
  SCantRemoveDereference = 'Ne peut retirer de déréférencement';

  // Erreurs d'instructions assembleur
  SMultipleParamsWithSameSepiStackOffset =
    'Plusieurs paramètres ont la même valeur de SepiStackOffset';
  SParamsSepiStackOffsetsDontFollow =
    'Les SepiStackOffset des paramètres ne se suivent pas';
  SInvalidDataSize = 'Taille de données invalide';
  SObjectMustHaveASignature = 'L''objet %s n''a pas de signature';

implementation

end.

