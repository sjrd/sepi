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
  Définit les constantes du package SepiBinaries
  @author sjrd
  @version 1.0
*}
unit SepiReflectionConsts;

interface

resourcestring
  SSepiMetaAlreadyCreated = 'Le meta de nom ''%s'' a déjà été créé';
  SSepiMetaAlreadyExists = 'Un meta de  nom ''%s'' existe déjà';
  SSepiMetaAlreadyAssigned = 'L''objet meta à la position %d est déjà assigné';
  SSepiObjectNotFound = 'Objet %s non trouvé';
  SSepiUnitNotFound = 'Impossible de trouver l''unité %s';
  SSepiNoRegisterTypeInfo = 'Ce type n''implémente pas RegisterTypeInfo';
  SCantCloneType = 'Ne peut cloner le type %s';
  SSepiBadConstType = 'Impossible de créer une constante de type variant %d';

  SSepiUnsupportedIntfCallConvention =
    'La convention d''appel %s n''est pas encore supportée pour les interfaces';

implementation

end.

