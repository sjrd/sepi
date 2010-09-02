{-------------------------------------------------------------------------------
Sepi - Object-oriented script engine for Delphi
Copyright (C) 2006-2009  S�bastien Doeraene
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
  D�finit les constantes du package SepiBinaries
  @author sjrd
  @version 1.0
*}
unit SepiReflectionConsts;

interface

const
  SUntypedTypeName = '$Untyped'; /// Nom du type non typ�

resourcestring
  SSepiComponentAlreadyCreated = 'Le composant de nom ''%s'' a d�j� �t� cr��';
  SSepiComponentAlreadyExists = 'Un composant de nom ''%s'' existe d�j�';
  SSepiComponentAlreadyAssigned =
    'Le composant � la position %d est d�j� assign�';
  SSepiComponentNotFound = 'Composant %s non trouv�';
  SSepiUnitNotFound = 'Impossible de trouver l''unit� %s';
  SSepiCantSaveLazyLoadUnit =
    'Impossible d''enregistrer une unit� en mode lazy-load';
  SSepiIncompatibleUsedUnitComponentNotFound =
    'L''unit� ''%s'' a �t� compil�e avec une version diff�rente de l''unit� '+
    'utilis�e ''%s'' (composant %s non trouv�), et doit �tre recompil�e.';
  SSepiIncompatibleUsedUnitIncompatibleComponent =
    'L''unit� ''%s'' a �t� compil�e avec une version diff�rente de l''unit� '+
    'utilis�e ''%s'' (composant %s incompatible), et doit �tre recompil�e.';
  SCantCloneType = 'Impossible de cloner le type %s';
  SSepiBadConstType = 'Impossible de cr�er une constante de type variant %d';
  SSignatureAlreadyCompleted = 'La signature est d�j� compl�t�e';

  SSepiUnsupportedIntfCallConvention =
    'La convention d''appel %s n''est pas encore support�e pour les interfaces';

  SUntypedTypeDescription = '(non typ�)';

  SUnknownValue = '(%s)';
  SOutOfBoundsEnumValue = 'Hors bornes (%d)';

implementation

end.

