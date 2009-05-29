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
  Constantes pour la compilation d'unité unité Delphi en Sepi
  @author sjrd
  @version 1.0
*}
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
  SOpenArrayParamCantHaveDefaultValue =
    'Un paramètre tableau ouvert ne peut pas avoir de valeur par défaut';
  SMultiNameParamCantHaveDefaultValue =
    'Une définition de plusieurs paramètres ne peut pas avoir de valeur par '+
    'défaut';
  SDuplicateModifier = 'Modificateur %s dupliqué';
  SPropertyNotFoundInBaseClass =
    'La propriété n''a pas été trouvée dans la classe de base';
  SFieldOrMethodRequired = 'Champ ou méthode requis';
  SArrayPropertyRequired = 'Propriété tableau requise';
  SDuplicateDefaultProperty = 'Ne peut avoir plusieurs propriétés par défaut';
  SParametersMismatch = 'Les paramètres ne correspondent pas';
  SMethodNotDeclared = 'Méthode %s non déclarée';
  SMethodMustBeOverloaded =
    'La méthode %s doit être marquée avec la directive overload';
  SPreviousDeclWasNotOverload =
    'La déclaration précédente de %s n''a pas été marquée avec la directive '+
    'overload';
  SDeclarationDiffersFromPreviousOne =
    'La déclaration de %s diffère de la déclaration précédente';
  SMethodAlreadyImplemented = 'La méthode %s a déjà été implémentée';

implementation

end.

