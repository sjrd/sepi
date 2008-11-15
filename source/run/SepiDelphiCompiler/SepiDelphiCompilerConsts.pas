unit SepiDelphiCompilerConsts;

interface

uses
  SepiReflectionCore, SepiMembers;

resourcestring
  SWrongSetCompType =
    'Les éléments d''un ensemble doivent être des ordinaux avec maximum 256 '+
    'valeurs';
  SUnknownMethodRefModifier =
    'Modificateur de référence de méthode %s inconnu';
  SUnknownMethodModifier = 'Modificateur de méthode %s inconnu';
  SUnknownPropertyModifier = 'Modificateur de propriété %s inconnu';
  SIntfMethodCantBeOverloaded =
    'Les méthodes d''interface ne peuvent être surchargées';
  SArrayValueRequired = 'Type tableau requis';
  SRecordValueRequired = 'Type record requis';
  SElementCountMismatch =
    'Le nombre d''éléments (%1:d) diffère de la déclaration (%0:d)';
  STypeIdentifierRequired = 'Identificateur de type requis';
  SOneParamRequiredForCast = 'Un paramètre requis pour un transtypage';
  SReturnTypeRequired = 'La fonction exige un type de retour';
  SReturnTypeForbidden = 'La procédure ne peut avoir de type de retour';
  SOpenArrayParamCantHaveDefaultValue =
    'Un paramètre tableau ouvert ne peut pas avoir de valeur par défaut';
  SMultiNameParamCantHaveDefaultValue =
    'Une définition de plusieurs paramètres ne peut pas avoir de valeur par '+
    'défaut';
  SDuplicateModifier = 'Modificateur %s dupliqué';
  SPropertyNotFoundInBaseClass =
    'La propriété n''a pas été trouvée dans la classe de base';
  SFieldOrMethodRequired = 'Champ ou méthode requis';
  SParametersMismatch = 'Les paramètres ne correspondent pas';
  SArrayOrArrayPropRequired = 'Valeur tableau ou propriété tableau requise';
  STooManyArrayIndices = 'Trop d''index pour ce tableau';
  SMethodNotDeclared = 'Méthode %s non déclarée';
  SMethodMustBeOverloaded =
    'La méthode %s doit être marquée avec la directive overload';
  SPreviousDeclWasNotOverload =
    'La déclaration précédente de %s n''a pas été marquée avec la directive '+
    'overload';
  SDeclarationDiffersFromPreviousOne =
    'La déclaration de %s diffère de la déclaration précédente';
  SMethodAlreadyImplemented = 'La méthode %s a déjà été implémentée';
  SLocalVarNameRequired = 'Variable locale requise';

const
  /// Nom des types de liaison de méthodes
  LinkKindNames: array[TMethodLinkKind] of string = (
    '', 'virtual', 'dynamic', 'message', '', 'override'
  );

  /// Nom des conventions d'appel
  CallingConventionNames: array[TCallingConvention] of string = (
    'register', 'cdecl', 'pascal', 'stdcall', 'safecall'
  );

  /// Nom des visibilités
  Visibilities: array[TMemberVisibility] of string = (
    'strict private', 'private', 'strict protected', 'protected', 'public',
    'published'
  );

implementation

end.

