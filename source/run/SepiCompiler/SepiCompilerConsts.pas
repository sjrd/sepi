{-------------------------------------------------------------------------------
Sepi - Object-oriented script engine for Delphi
Copyright (C) 2006-2007  Sébastien Doeraene
All Rights Reserved

This file is part of Sepi.

Sepi is free software: you can redistribute it and/or modify it under the terms
of the GNU General Public License as published by the Free Software Foundation,
either version 3 of the License, or (at your option) any later version.

Sepi is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
Sepi.  If not, see <http://www.gnu.org/licenses/>.
-------------------------------------------------------------------------------}

{*
  Constantes utilisées dans un compilateur Sepi
  @author sjrd
  @version 1.0
*}
unit SepiCompilerConsts;

interface

resourcestring
  // Format des erreurs
  SSepiHintName = 'Conseil';
  SSepiWarningName = 'Avertissement';
  SSepiErrorName = 'Erreur';
  SSepiFatalErrorName = 'Erreur fatale';
  SSepiCompilerErrorFormat = '[%s] %s(%d, %d) : %s';

  // Erreurs de compilation génériques
  SCantOpenSourceFile = 'Ne peut ouvrir le fichier source %s';
  SCantOpenDestFile = 'Ne peut ouvrir le fichier de destination %s';
  SSepiInternalError = 'Erreur interne : %s';
  SSepiThereWereErrors = 'Il y a eu des erreurs à la compilation';
  STooManyErrors = 'Trop d''erreurs';
  SIdentifier = 'Identificateur';
  SRedeclaredIdentifier = 'Identificateur redéclaré';
  SIdentifierNotFound = 'Identificateur %s non déclaré';
  STypeMismatch = 'Types incompatibles : %s et %s';
  SNeedConstExpression = 'Expression constante attendue';

  // Erreurs de l'analyseur lexical
  SBadSourceCharacter = 'Caractère %s incorrect dans un source';
  SEndOfFile = 'Fin de fichier';
  SStringNotTerminated = 'Chaîne non terminée';

  // Erreurs de l'analyseur syntaxique
  SSyntaxError = '%s attendu mais %s trouvé';

  // Erreurs de compilation sur les expressions
  SSepiErroneousTypeName = 'type erroné';
  STypeIsNotBaseType =
    'Le type %s n''est pas un type de base pour l''interpréteur Sepi';
  SInvalidCast = 'Transtypage invalide de %s en %s';
  SOperationNotApplicableToType =
    'Opération non applicable à ce type d''opérande';
  STypeHasNoTypeInfo = 'Ce type n''a pas d''informations de type';
  STestValueIsAlways = 'La condition est toujours évaluée à %s';
  SConstExpressionRequired = 'Expression constante attendue';
  SMethodRequired = 'Identificateur de méthode requis';
  SInheritNeedClassOrObjectMethod =
    'Appel de type inherited invalide hors d''une méthode d''objet ou de '+
    'classe';
  SCallableRequired = 'Expression invocable requise';
  SMetaRequired = 'Nom d''objet requis';

  // Sorte particulière de type requise
  STypeIdentifierRequired = 'Identificateur de type requis';
  SPointerTypeRequired = 'Type pointeur requis';
  SOrdinalOrArrayTypeRequired = 'Type ordinal ou tableau requis';
  SArrayTypeRequired = 'Type tableau requis';
  SOrdinalTypeRequired = 'Type ordinal requis';
  SClassTypeRequired = 'Type classe requis';
  SInterfaceTypeRequired = 'Type interface requis';

  // Erreurs sur les valeurs
  SValueRequired = 'Valeur requise';
  SVarValueRequired = 'Variable requise';
  SValueCantBeRead = 'La valeur ne peut être lue';
  SReadableValueRequired = 'Valeur requise';
  SWritableValueRequired = 'La valeur ne peut être écrite';
  SAddressableValueRequired = 'Valeur adressable requise';

  // Erreurs sur les ensembles
  SUntypedEmptySetNotSupported = 'Ensemble vide non typé non supporté';
  SSetRangeTooWide = 'L''ensemble des valeurs est trop étendu';
  SConstValueOutOfBounds =
    'La valeur constante dépasse les limites de sous-étendue';
  SCompTypeTooNarrow =
    'Le type d''élément %s est trop petit pour contenir toutes les valeurs '+
    'possibles de cet ensemble';

  // Erreurs sur les signatures
  SReturnTypeRequired = 'La fonction exige un type de retour';
  SReturnTypeForbidden = 'La procédure ne peut avoir de type de retour';

  // Erreurs sur les paramètres
  SVarParamTypeMustBeStrictlyEqual =
    'Les paramètres var originaux et formels doivent avoir le même type';
  SNotEnoughActualParameters = 'Pas assez de paramètres réels';
  STooManyActualParameters = 'Trop de paramètres réels';
  SNoMatchingOverloadedMethod =
    'Aucune méthode surchargée ne peut être invoquée avec ces paramètres';
  SCallPatternOnlyOnClassMethod =
    'Forme d''appel autorisée uniquement sur les méthodes de classe';

  // Erreurs sur les variables locales
  SLocalVarNameRequired = 'Variable locale requise';

  // Erreurs sur les indices de tableau
  SArrayOrArrayPropRequired = 'Valeur tableau ou propriété tableau requise';
  STooManyArrayIndices = 'Trop d''index pour ce tableau';

  // Erreurs sur des classes ou interfaces
  SClassDoesNotImplementIntf = 'La classe %s n''implémente pas l''interface %s';

  // Erreurs sur les jump spéciaux
  SContinueAllowedOnlyInLoop =
    'L''instruction Continue n''est autorisée que dans une boucle';
  SBreakAllowedOnlyInLoop =
    'L''instruction Break n''est autorisée que dans une boucle';

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
  SParamsAlreadyCompleted = 'Les paramètres ont déjà été complétés';
  SSignatureAlreadyKnown = 'La signature est déjà connue';

  // Erreurs d'instructions assembleur
  SMultipleParamsWithSameSepiStackOffset =
    'Plusieurs paramètres ont la même valeur de SepiStackOffset';
  SParamsSepiStackOffsetsDontFollow =
    'Les SepiStackOffset des paramètres ne se suivent pas';
  SInvalidDataSize = 'Taille de données invalide';
  SObjectMustHaveASignature = 'L''objet %s n''a pas de signature';

  // Erreurs de parser LL1
  STopOfStackIsNotASymbol = 'Le sommet de la pile n''est pas un symbole';
  STopOfStackIsNotATry = 'Le sommet de la pile n''est pas un try';
  SNotInTry = 'La pile n''est pas dans un try';

implementation

end.

