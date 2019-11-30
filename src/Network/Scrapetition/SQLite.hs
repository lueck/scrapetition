module Network.Scrapetition.SQLite
  ()
where

-- * Add some more instances.

import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.FromField


instance (ToField a, ToField b, ToField c, ToField d, ToField e,
          ToField f, ToField g, ToField h, ToField i, ToField j,
          ToField k)
    => ToRow (a,b,c,d,e,f,g,h,i,j, k) where
    toRow (a,b,c,d,e,f,g,h,i,j, k) =
        [toField a, toField b, toField c, toField d, toField e,
         toField f, toField g, toField h, toField i, toField j,
         toField k]

instance (ToField a, ToField b, ToField c, ToField d, ToField e,
          ToField f, ToField g, ToField h, ToField i, ToField j,
          ToField k, ToField l)
    => ToRow (a,b,c,d,e,f,g,h,i,j, k,l) where
    toRow (a,b,c,d,e,f,g,h,i,j, k,l) =
        [toField a, toField b, toField c, toField d, toField e,
         toField f, toField g, toField h, toField i, toField j,
         toField k, toField l]


instance (FromField a, FromField b, FromField c, FromField d, FromField e,
          FromField f, FromField g, FromField h, FromField i, FromField j,
          FromField k) =>
    FromRow (a,b,c,d,e,f,g,h,i,j, k) where
    fromRow = (,,,,,,,,,,)
      <$> field <*> field <*> field <*> field <*> field
      <*> field <*> field <*> field <*> field <*> field
      <*> field

instance (FromField a, FromField b, FromField c, FromField d, FromField e,
          FromField f, FromField g, FromField h, FromField i, FromField j,
          FromField k, FromField l) =>
    FromRow (a,b,c,d,e,f,g,h,i,j, k, l) where
    fromRow = (,,,,,,,,,,,)
      <$> field <*> field <*> field <*> field <*> field
      <*> field <*> field <*> field <*> field <*> field
      <*> field <*> field

instance (FromField a, FromField b, FromField c, FromField d, FromField e,
          FromField f, FromField g, FromField h, FromField i, FromField j,
          FromField k, FromField l, FromField m) =>
    FromRow (a,b,c,d,e,f,g,h,i,j, k, l, m) where
    fromRow = (,,,,,,,,,,,,)
      <$> field <*> field <*> field <*> field <*> field
      <*> field <*> field <*> field <*> field <*> field
      <*> field <*> field <*> field

instance (FromField a, FromField b, FromField c, FromField d, FromField e,
          FromField f, FromField g, FromField h, FromField i, FromField j,
          FromField k, FromField l, FromField m, FromField n) =>
    FromRow (a,b,c,d,e,f,g,h,i,j, k, l, m, n) where
    fromRow = (,,,,,,,,,,,,,)
      <$> field <*> field <*> field <*> field <*> field
      <*> field <*> field <*> field <*> field <*> field
      <*> field <*> field <*> field <*> field

