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
  SSepiCompilerErrorFormat = '[%s] %s(%d) : %4:s';

  // Erreurs de compilation génériques
  SSepiThereWereErrors = 'Il y a eu des erreurs à la compilation';
  SIdentifier = 'Identificateur';
  SRedeclaredIdentifier = 'Identificateur redéclaré';
  SIdentifierNotFound = 'Identificateur %s non déclaré';
  STypeMismatch = 'Types incompatibles : %s et %s';
  SNeedConstExpression = 'Expression constante attendue';

  // Erreurs de compilation sur les expressions
  STypeIsNotBaseType =
    'Le type %s n''est pas un type de base pour l''interpréteur Sepi';
  SInvalidCast = 'Transtypage invalide de %s en %s';
  SOperationNotApplicableToType =
    'Opération non applicable à ce type d''opérande';
  SNeedPointerType = 'Type pointeur requis';
  STypeHasNoTypeInfo = 'Ce type n''a pas d''informations de type';
  SOrdinalOrArrayTypeRequired = 'Type ordinal ou tableau requis';
  SArrayTypeRequired = 'Type tableau requis';
  STestValueIsAlways = 'La condition est toujours évaluée à %s';
  SOrdinalTypeRequired = 'Type ordinal requis';
  SClassTypeRequired = 'Type classe requis';
  SAddressableValueRequired = 'Valeur adressable requise';
  SValueCantBeRead = 'La valeur ne peut être lue';
  SVarParamTypeMustBeStrictlyEqual =
    'Les paramètres var originaux et formels doivent avoir le même type';
  SNotEnoughActualParameters = 'Pas assez de paramètres réels';
  STooManyActualParameters = 'Trop de paramètres réels';

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

implementation

end.

