{-------------------------------------------------------------------------------
Sepi - Object-oriented script engine for Delphi
Copyright (C) 2006-2009  Sébastien Doeraene
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

Linking this library statically or dynamically with other modules is making a
combined work based on this library.  Thus, the terms and conditions of the GNU
General Public License cover the whole combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent modules, and
to copy and distribute the resulting executable under terms of your choice,
provided that you also meet, for each linked independent module, the terms and
conditions of the license of that module.  An independent module is a module
which is not derived from or based on this library.  If you modify this
library, you may extend this exception to your version of the library, but you
are not obligated to do so.  If you do not wish to do so, delete this exception
statement from your version.
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
  SBookmarksCantPassThroughLexer =
    'Impossible de revenir en arrière en changeant d''analyseur lexical de '+
    'base.';

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
  SExecutableRequired = 'Expression exécutable requise';
  SComponentRequired = 'Nom d''objet requis';
  SIdentifierRequired = 'Identificateur requis';

  // Sorte particulière de type requise
  STypeIdentifierRequired = 'Identificateur de type requis';
  SStringTypeRequired = 'Type chaîne requis';
  SPointerTypeRequired = 'Type pointeur requis';
  SArrayTypeRequired = 'Type tableau requis';
  SDynArrayTypeRequired = 'Type tableau dynamique requis';
  SStringOrDynArrayTypeRequired = 'Type chaîne ou tableau dynamique requis';
  SRecordTypeRequired = 'Type record requis';
  SOrdinalOrArrayTypeRequired = 'Type ordinal ou tableau requis';
  SOrdinalTypeRequired = 'Type ordinal requis';
  SSetTypeRequired = 'Type ensemble requis';
  SClassTypeRequired = 'Type classe requis';
  SInterfaceTypeRequired = 'Type interface requis';
  SMetaClassTypeRequired = 'Type méta-classe requis';
  SContainerTypeRequired = 'Type classe, interface ou record requis';
  SCompilerTransientTypeForbidden =
    'Type de valeur spécial du compilateur non autorisé ici';
  SOpenArrayRequired = 'Tableau ouvert de type %s requis';
  SInvalidArrayOfConstItem =
    'Type de valeur non autorisé dans un élément de tableau ouvert';
  SStringOrArrayTypeRequired = 'Type chaîne ou tableau requis';
  SStringTypeOrTypeIdentifierRequired =
    'Type chaîne ou identificateur de type requis';
  SArrayTypeOrTypeIdentifierRequired =
    'Type tableau ou identificateur de type requis';
  SStringOrArrayTypeOrTypeIdentifierRequired =
    'Type tableau, type chaîne, ou identificateur de type requis';

  // Erreurs sur les valeurs
  SValueRequired = 'Valeur requise';
  SVarValueRequired = 'Variable requise';
  SValueCantBeRead = 'La valeur ne peut être lue';
  SReadableValueRequired = 'Valeur requise';
  SWritableValueRequired = 'La valeur ne peut être écrite';
  SAddressableValueRequired = 'Valeur adressable requise';

  // Valeurs particulières requise
  SBadStringLength =
    'La taille des chaînes doit être comprise entre 0 et 255 inclus';

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
  SIntfMethodCantBeOverloaded =
    'Les méthodes d''interface ne peuvent être surchargées';
  SIntfMethodCantChangeLinkKind =
    'Type de liaison invalide pour une méthode d''interface';
  SDuplicatedLinkKind = 'Directive de type de liaison dupliquée';
  SDuplicatedAbstractMarker = 'Directive abstract dupliquée';
  SDuplicatedOfObjectMarker = 'Directive of object dupliquée';
  SVirtualOrDynamicMethodRequired = 'Méthode virtuelle ou dynamique requise';
  SMethodNotFoundInBaseClass = 'Méthode non trouvée dans la classe de base';

  // Erreurs sur les paramètres
  SVarParamTypeMustBeStrictlyEqual =
    'Les paramètres var originaux et formels doivent avoir le même type';
  SNotEnoughActualParameters = 'Pas assez de paramètres réels';
  STooManyActualParameters = 'Trop de paramètres réels';
  SNoMatchingOverloadedMethod =
    'Aucune méthode surchargée ne peut être invoquée avec ces paramètres';
  SCallPatternOnlyOnClassMethod =
    'Forme d''appel autorisée uniquement sur les méthodes de classe';
  SOpenArrayParamCantHaveDefaultValue =
    'Un paramètre tableau ouvert ne peut pas avoir de valeur par défaut';
  SMultiNameParamCantHaveDefaultValue =
    'Une définition de plusieurs paramètres ne peut pas avoir de valeur par '+
    'défaut';
  SByRefParamCantHaveDefaultValue =
    'Un paramètre passé par référence ne peut pas avoir de valeur par défaut';

  // Erreurs sur les propriété
  SInvalidStorageValue = 'Spécificateur de stockage invalide';

  // Erreurs sur les variables locales
  SLocalVarNameRequired = 'Variable locale requise';

  // Erreurs sur l'implémentation des méthodes
  SMethodNotDeclared = 'Méthode %s non déclarée';
  SMethodMustBeOverloaded =
    'La méthode %s doit être marquée avec la directive overload';
  SPreviousDeclWasNotOverload =
    'La déclaration précédente de %s n''a pas été marquée avec la directive '+
    'overload';
  SDeclarationDiffersFromPreviousOne =
    'La déclaration de %s diffère de la déclaration précédente';
  SMethodAlreadyImplemented = 'La méthode %s a déjà été implémentée';

  // Erreurs sur les indices de tableau
  SArrayOrArrayPropRequired = 'Valeur tableau ou propriété tableau requise';
  STooManyArrayIndices = 'Trop d''index pour ce tableau';

  // Erreurs sur des classes ou interfaces
  SClassDoesNotImplementIntf = 'La classe %s n''implémente pas l''interface %s';
  SPropertyNotFoundInBaseClass =
    'La propriété n''a pas été trouvée dans la classe de base';
  SFieldOrMethodRequired = 'Champ ou méthode requis';
  SFieldRequired = 'Champ requis';
  SRecordFieldRequired = 'Champ de type record requis';
  SScalarPropertyRequired = 'Propriété non-tableau requise';
  SDuplicateDefaultDirective = 'Directive default dupliquée';
  SArrayPropertyRequired = 'Propriété tableau requise';
  SDuplicateDefaultProperty = 'Ne peut avoir plusieurs propriétés par défaut';
  SParametersMismatch = 'Les paramètres ne correspondent pas';

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

