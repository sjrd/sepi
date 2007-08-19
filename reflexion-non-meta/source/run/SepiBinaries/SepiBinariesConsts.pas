{*
  Définit les constantes du package SepiBinaries
  @author sjrd
  @version 1.0
*}
unit SepiBinariesConsts;

interface

resourcestring
  SSepiMetaAlreadyCreated = 'Le meta de nom ''%s'' a déjà été créé';
  SSepiMetaAlreadyExists = 'Un meta de  nom ''%s'' existe déjà';
  SSepiMetaAlreadyAssigned = 'L''objet meta à la position %d est déjà assigné';
  SSepiObjectNotFound = 'Objet %s non trouvé';
  SSepiUnitNotFound = 'Impossible de trouver l''unité %s';
  SSepiNoRegisterTypeInfo = 'Ce type n''implémente pas RegisterTypeInfo';
  SSepiBadConstType = 'Impossible de créer une constante de type variant %d';

  SSepiUnsupportedIntfCallConvention =
    'La convention d''appel %s n''est pas encore supportée pour les interfaces';

implementation

end.

