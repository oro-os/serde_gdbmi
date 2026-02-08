use crate::Error;
use crate::parser::{
    DataSymbol, Response, ResponseBody as ParserResponseBody, StreamSymbol, Value,
};
use serde::de::{self, DeserializeSeed, EnumAccess, MapAccess, SeqAccess, VariantAccess, Visitor};
use std::collections::HashMap;

/*
    NOTE(qix-): A bit of a disclaimer, this file was written almost entirely with AI.
    NOTE(qix-): If that's a problem for you, I understand.
    NOTE(qix-):
    NOTE(qix-): The rest of the crate was completely hand-written with AI completely
    NOTE(qix-): disabled (not for any philosophical reason, more that it was annoying
    NOTE(qix-): me, and I knew exactly how I wanted this to be written), and only had
    NOTE(qix-): a few test cases written here before I let AI go nuts with it.
    NOTE(qix-):
    NOTE(qix-): It's glue code, between the parser I wrote and the `serde` library,
    NOTE(qix-): which I simply didn't have time to deep dive into the internals
    NOTE(qix-): to learn well enough to write my own deserializer here. I needed
    NOTE(qix-): this library finished quickly, for another project, and figured
    NOTE(qix-): that the _bones_ are strong enough that the AI glue would be fine
    NOTE(qix-): enough for an initial release.
    NOTE(qix-):
    NOTE(qix-): Anyway, that also means if anything in here is horrifying serde-wise,
    NOTE(qix-): please open an issue and I'll address it when I get some more time.
*/

/// Simple string deserializer to avoid ambiguity issues
struct StrDeserializer<'a>(&'a str);

impl<'de, 'a: 'de> de::Deserializer<'de> for StrDeserializer<'a> {
    type Error = Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_borrowed_str(self.0)
    }

    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_borrowed_str(self.0)
    }

    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_borrowed_str(self.0)
    }

    fn deserialize_identifier<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_borrowed_str(self.0)
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 u8 u16 u32 u64 f32 f64 char bytes
        byte_buf option unit unit_struct newtype_struct seq tuple
        tuple_struct map struct enum ignored_any
    }
}

/// A `serde` deserializer that allows for deserializing MI responses
/// into structures.
pub struct Deserializer<'a> {
    response: &'a Response,
}

impl<'a> Deserializer<'a> {
    pub fn from_response(response: &'a Response) -> Self {
        Self { response }
    }
}

/// Deserializer for individual values
struct ValueDeserializer<'a> {
    value: &'a Value,
}

impl<'a> ValueDeserializer<'a> {
    fn new(value: &'a Value) -> Self {
        Self { value }
    }
}

/// Deserializer for data bodies (class + variables)
pub struct DataDeserializer<'a> {
    class: &'a str,
    variables: &'a HashMap<String, Value>,
}

impl<'a> DataDeserializer<'a> {
    pub fn from_data(class: &'a str, variables: &'a HashMap<String, Value>) -> Self {
        Self { class, variables }
    }
}

/// Token deserializer
pub struct TokenDeserializer<'a> {
    token: &'a str,
}

impl<'a> TokenDeserializer<'a> {
    #[expect(
        clippy::should_implement_trait,
        reason = "let's not encourage people pass this to `impl FromStr` arguments"
    )]
    pub fn from_str(token: &'a str) -> Self {
        Self { token }
    }
}

impl<'de, 'a: 'de> de::Deserializer<'de> for TokenDeserializer<'a> {
    type Error = Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_borrowed_str(self.token)
    }

    fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let val = self.token.parse::<u64>().map_err(|_| Error::InvalidType {
            expected: "u64".to_string(),
            got: self.token.to_string(),
        })?;
        visitor.visit_u64(val)
    }

    fn deserialize_i64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let val = self.token.parse::<i64>().map_err(|_| Error::InvalidType {
            expected: "i64".to_string(),
            got: self.token.to_string(),
        })?;
        visitor.visit_i64(val)
    }

    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_borrowed_str(self.token)
    }

    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_borrowed_str(self.token)
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 u8 u16 u32 f32 f64 char bytes
        byte_buf option unit unit_struct newtype_struct seq tuple
        tuple_struct map struct enum identifier ignored_any
    }
}

// Implement ValueDeserializer - deserializes from parser::Value
impl<'de, 'a: 'de> de::Deserializer<'de> for ValueDeserializer<'a> {
    type Error = Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self.value {
            Value::String(s) => visitor.visit_borrowed_str(s),
            Value::List(_) => self.deserialize_seq(visitor),
            Value::Dict(_) => self.deserialize_map(visitor),
        }
    }

    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self.value {
            Value::String(s) => {
                let b = s.parse::<bool>().map_err(|_| Error::InvalidType {
                    expected: "bool".to_string(),
                    got: s.clone(),
                })?;
                visitor.visit_bool(b)
            }
            _ => Err(Error::InvalidType {
                expected: "bool".to_string(),
                got: format!("{:?}", self.value),
            }),
        }
    }

    fn deserialize_i8<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_integer(visitor)
    }

    fn deserialize_i16<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_integer(visitor)
    }

    fn deserialize_i32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_integer(visitor)
    }

    fn deserialize_i64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_integer(visitor)
    }

    fn deserialize_u8<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_integer(visitor)
    }

    fn deserialize_u16<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_integer(visitor)
    }

    fn deserialize_u32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_integer(visitor)
    }

    fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_integer(visitor)
    }

    fn deserialize_f32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self.value {
            Value::String(s) => {
                let f = s.parse::<f32>().map_err(|_| Error::InvalidType {
                    expected: "f32".to_string(),
                    got: s.clone(),
                })?;
                visitor.visit_f32(f)
            }
            _ => Err(Error::InvalidType {
                expected: "f32".to_string(),
                got: format!("{:?}", self.value),
            }),
        }
    }

    fn deserialize_f64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self.value {
            Value::String(s) => {
                let f = s.parse::<f64>().map_err(|_| Error::InvalidType {
                    expected: "f64".to_string(),
                    got: s.clone(),
                })?;
                visitor.visit_f64(f)
            }
            _ => Err(Error::InvalidType {
                expected: "f64".to_string(),
                got: format!("{:?}", self.value),
            }),
        }
    }

    fn deserialize_char<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self.value {
            Value::String(s) => {
                let mut chars = s.chars();
                let c = chars.next().ok_or_else(|| Error::InvalidType {
                    expected: "char".to_string(),
                    got: s.clone(),
                })?;
                if chars.next().is_some() {
                    return Err(Error::InvalidType {
                        expected: "char".to_string(),
                        got: s.clone(),
                    });
                }
                visitor.visit_char(c)
            }
            _ => Err(Error::InvalidType {
                expected: "char".to_string(),
                got: format!("{:?}", self.value),
            }),
        }
    }

    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self.value {
            Value::String(s) => visitor.visit_borrowed_str(s),
            _ => Err(Error::InvalidType {
                expected: "string".to_string(),
                got: format!("{:?}", self.value),
            }),
        }
    }

    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_str(visitor)
    }

    fn deserialize_bytes<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self.value {
            Value::String(s) => visitor.visit_borrowed_bytes(s.as_bytes()),
            _ => Err(Error::InvalidType {
                expected: "bytes".to_string(),
                got: format!("{:?}", self.value),
            }),
        }
    }

    fn deserialize_byte_buf<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_bytes(visitor)
    }

    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_some(self)
    }

    fn deserialize_unit<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_unit()
    }

    fn deserialize_unit_struct<V>(
        self,
        _name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_unit(visitor)
    }

    fn deserialize_newtype_struct<V>(
        self,
        _name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_newtype_struct(self)
    }

    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self.value {
            Value::List(list) => visitor.visit_seq(SeqDeserializer::new(list)),
            _ => Err(Error::InvalidType {
                expected: "sequence".to_string(),
                got: format!("{:?}", self.value),
            }),
        }
    }

    fn deserialize_tuple<V>(self, _len: usize, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_seq(visitor)
    }

    fn deserialize_tuple_struct<V>(
        self,
        _name: &'static str,
        _len: usize,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_seq(visitor)
    }

    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self.value {
            Value::Dict(dict) => visitor.visit_map(MapDeserializer::new(dict)),
            _ => Err(Error::InvalidType {
                expected: "map".to_string(),
                got: format!("{:?}", self.value),
            }),
        }
    }

    fn deserialize_struct<V>(
        self,
        _name: &'static str,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_map(visitor)
    }

    fn deserialize_enum<V>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self.value {
            Value::String(s) => visitor.visit_enum(StringEnumAccess(s.as_str())),
            Value::Dict(dict) => {
                // For internally tagged enums
                visitor.visit_enum(ValueEnumAccess::new(dict))
            }
            _ => Err(Error::InvalidType {
                expected: "enum".to_string(),
                got: format!("{:?}", self.value),
            }),
        }
    }

    fn deserialize_identifier<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_str(visitor)
    }

    fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_unit()
    }
}

impl<'a> ValueDeserializer<'a> {
    fn deserialize_integer<'de, V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: Visitor<'de>,
    {
        match self.value {
            Value::String(s) => {
                // Try parsing as different integer types
                if let Ok(i) = s.parse::<i64>() {
                    return visitor.visit_i64(i);
                }
                if let Ok(u) = s.parse::<u64>() {
                    return visitor.visit_u64(u);
                }
                Err(Error::InvalidType {
                    expected: "integer".to_string(),
                    got: s.clone(),
                })
            }
            _ => Err(Error::InvalidType {
                expected: "integer".to_string(),
                got: format!("{:?}", self.value),
            }),
        }
    }
}

// String enum access for unit variant enums
struct StringEnumAccess<'a>(&'a str);

impl<'de, 'a: 'de> EnumAccess<'de> for StringEnumAccess<'a> {
    type Error = Error;
    type Variant = UnitVariantAccess;

    fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Self::Variant), Self::Error>
    where
        V: DeserializeSeed<'de>,
    {
        let variant = seed.deserialize(StrDeserializer(self.0))?;
        Ok((variant, UnitVariantAccess))
    }
}

struct UnitVariantAccess;

impl<'de> VariantAccess<'de> for UnitVariantAccess {
    type Error = Error;

    fn unit_variant(self) -> Result<(), Self::Error> {
        Ok(())
    }

    fn newtype_variant_seed<T>(self, _seed: T) -> Result<T::Value, Self::Error>
    where
        T: DeserializeSeed<'de>,
    {
        Err(Error::Custom("expected unit variant".to_string()))
    }

    fn tuple_variant<V>(self, _len: usize, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        Err(Error::Custom("expected unit variant".to_string()))
    }

    fn struct_variant<V>(
        self,
        _fields: &'static [&'static str],
        _visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        Err(Error::Custom("expected unit variant".to_string()))
    }
}

// Sequence deserializer
struct SeqDeserializer<'a> {
    iter: std::slice::Iter<'a, Value>,
}

impl<'a> SeqDeserializer<'a> {
    fn new(values: &'a [Value]) -> Self {
        Self {
            iter: values.iter(),
        }
    }
}

impl<'de, 'a: 'de> SeqAccess<'de> for SeqDeserializer<'a> {
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: DeserializeSeed<'de>,
    {
        match self.iter.next() {
            Some(value) => seed.deserialize(ValueDeserializer::new(value)).map(Some),
            None => Ok(None),
        }
    }
}

// Map deserializer
struct MapDeserializer<'a> {
    iter: std::collections::hash_map::Iter<'a, String, Value>,
    value: Option<&'a Value>,
}

impl<'a> MapDeserializer<'a> {
    fn new(map: &'a HashMap<String, Value>) -> Self {
        Self {
            iter: map.iter(),
            value: None,
        }
    }
}

impl<'de, 'a: 'de> MapAccess<'de> for MapDeserializer<'a> {
    type Error = Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Self::Error>
    where
        K: DeserializeSeed<'de>,
    {
        match self.iter.next() {
            Some((key, value)) => {
                self.value = Some(value);
                seed.deserialize(StrDeserializer(key.as_str())).map(Some)
            }
            None => Ok(None),
        }
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Self::Error>
    where
        V: DeserializeSeed<'de>,
    {
        match self.value.take() {
            Some(value) => seed.deserialize(ValueDeserializer::new(value)),
            None => Err(Error::Custom("value is missing".to_string())),
        }
    }
}

// Enum access for Value enums
struct ValueEnumAccess<'a> {
    dict: &'a HashMap<String, Value>,
}

impl<'a> ValueEnumAccess<'a> {
    fn new(dict: &'a HashMap<String, Value>) -> Self {
        Self { dict }
    }
}

impl<'de, 'a: 'de> EnumAccess<'de> for ValueEnumAccess<'a> {
    type Error = Error;
    type Variant = ValueVariantAccess<'a>;

    fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Self::Variant), Self::Error>
    where
        V: DeserializeSeed<'de>,
    {
        // For internally tagged enums, we need to find the tag field
        // For now, we'll use the first key as the variant
        if let Some((key, _)) = self.dict.iter().next() {
            let variant = seed.deserialize(StrDeserializer(key.as_str()))?;
            Ok((variant, ValueVariantAccess::new(self.dict)))
        } else {
            Err(Error::Custom("empty enum".to_string()))
        }
    }
}

struct ValueVariantAccess<'a> {
    dict: &'a HashMap<String, Value>,
}

impl<'a> ValueVariantAccess<'a> {
    fn new(dict: &'a HashMap<String, Value>) -> Self {
        Self { dict }
    }
}

impl<'de, 'a: 'de> VariantAccess<'de> for ValueVariantAccess<'a> {
    type Error = Error;

    fn unit_variant(self) -> Result<(), Self::Error> {
        Ok(())
    }

    fn newtype_variant_seed<T>(self, seed: T) -> Result<T::Value, Self::Error>
    where
        T: DeserializeSeed<'de>,
    {
        // For newtype variants, deserialize the dict as a struct
        seed.deserialize(MapToStructDeserializer::new(self.dict))
    }

    fn tuple_variant<V>(self, _len: usize, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        Err(Error::Custom(
            "tuple variants not supported for dict values".to_string(),
        ))
    }

    fn struct_variant<V>(
        self,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_map(MapDeserializer::new(self.dict))
    }
}

// Helper deserializer for deserializing a map as a struct
struct MapToStructDeserializer<'a> {
    dict: &'a HashMap<String, Value>,
}

impl<'a> MapToStructDeserializer<'a> {
    fn new(dict: &'a HashMap<String, Value>) -> Self {
        Self { dict }
    }
}

impl<'de, 'a: 'de> de::Deserializer<'de> for MapToStructDeserializer<'a> {
    type Error = Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_map(MapDeserializer::new(self.dict))
    }

    fn deserialize_struct<V>(
        self,
        _name: &'static str,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_map(MapDeserializer::new(self.dict))
    }

    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_map(MapDeserializer::new(self.dict))
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 u8 u16 u32 u64 f32 f64 char str string bytes
        byte_buf option unit unit_struct newtype_struct seq tuple
        tuple_struct enum identifier ignored_any
    }
}

// Implement DataDeserializer - used for deserializing user types from DataBody
impl<'de, 'a: 'de> de::Deserializer<'de> for DataDeserializer<'a> {
    type Error = Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        // For "any", we treat it as an enum
        self.deserialize_enum("", &[], visitor)
    }

    fn deserialize_enum<V>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_enum(DataEnumAccess::new(self.class, self.variables))
    }

    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_map(MapDeserializer::new(self.variables))
    }

    fn deserialize_struct<V>(
        self,
        _name: &'static str,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_map(visitor)
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 u8 u16 u32 u64 f32 f64 char str string bytes
        byte_buf option unit unit_struct newtype_struct seq tuple
        tuple_struct identifier ignored_any
    }
}

// Enum access for DataBody - matches class name to enum variant
struct DataEnumAccess<'a> {
    class: &'a str,
    variables: &'a HashMap<String, Value>,
}

impl<'a> DataEnumAccess<'a> {
    fn new(class: &'a str, variables: &'a HashMap<String, Value>) -> Self {
        Self { class, variables }
    }
}

impl<'de, 'a: 'de> EnumAccess<'de> for DataEnumAccess<'a> {
    type Error = Error;
    type Variant = DataVariantAccess<'a>;

    fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Self::Variant), Self::Error>
    where
        V: DeserializeSeed<'de>,
    {
        let variant = seed.deserialize(StrDeserializer(self.class))?;
        Ok((variant, DataVariantAccess::new(self.variables)))
    }
}

struct DataVariantAccess<'a> {
    variables: &'a HashMap<String, Value>,
}

impl<'a> DataVariantAccess<'a> {
    fn new(variables: &'a HashMap<String, Value>) -> Self {
        Self { variables }
    }
}

impl<'de, 'a: 'de> VariantAccess<'de> for DataVariantAccess<'a> {
    type Error = Error;

    fn unit_variant(self) -> Result<(), Self::Error> {
        Ok(())
    }

    fn newtype_variant_seed<T>(self, seed: T) -> Result<T::Value, Self::Error>
    where
        T: DeserializeSeed<'de>,
    {
        seed.deserialize(MapToStructDeserializer::new(self.variables))
    }

    fn tuple_variant<V>(self, _len: usize, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_seq(SeqDeserializer::new(&[]))
    }

    fn struct_variant<V>(
        self,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_map(MapDeserializer::new(self.variables))
    }
}

// Main Deserializer for Response<T>
impl<'de, 'a: 'de> de::Deserializer<'de> for &'a Deserializer<'a> {
    type Error = Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_struct("Response", &["token", "body"], visitor)
    }

    fn deserialize_struct<V>(
        self,
        _name: &'static str,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_map(ResponseMapAccess::new(self.response))
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 u8 u16 u32 u64 f32 f64 char str string bytes
        byte_buf option unit unit_struct newtype_struct seq tuple
        tuple_struct map enum identifier ignored_any
    }
}

// Map access for Response struct
struct ResponseMapAccess<'a> {
    response: &'a Response,
    state: ResponseDeserializeState,
}

#[derive(Debug, Clone, Copy)]
enum ResponseDeserializeState {
    Token,
    Body,
    Done,
}

impl<'a> ResponseMapAccess<'a> {
    fn new(response: &'a Response) -> Self {
        Self {
            response,
            state: ResponseDeserializeState::Token,
        }
    }
}

impl<'de, 'a: 'de> MapAccess<'de> for ResponseMapAccess<'a> {
    type Error = Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Self::Error>
    where
        K: DeserializeSeed<'de>,
    {
        match self.state {
            ResponseDeserializeState::Token => {
                self.state = ResponseDeserializeState::Body;
                seed.deserialize(StrDeserializer("token")).map(Some)
            }
            ResponseDeserializeState::Body => {
                self.state = ResponseDeserializeState::Done;
                seed.deserialize(StrDeserializer("body")).map(Some)
            }
            ResponseDeserializeState::Done => Ok(None),
        }
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Self::Error>
    where
        V: DeserializeSeed<'de>,
    {
        match self.state {
            ResponseDeserializeState::Body => {
                // Deserialize token
                seed.deserialize(OptionTokenDeserializer::new(self.response.token.as_deref()))
            }
            ResponseDeserializeState::Done => {
                // Deserialize body
                seed.deserialize(ResponseBodyDeserializer::new(&self.response.body))
            }
            ResponseDeserializeState::Token => Err(Error::Custom("invalid state".to_string())),
        }
    }
}

// Token deserializer for the optional token field in Response
struct OptionTokenDeserializer<'a> {
    token: Option<&'a str>,
}

impl<'a> OptionTokenDeserializer<'a> {
    fn new(token: Option<&'a str>) -> Self {
        Self { token }
    }
}

impl<'de, 'a: 'de> de::Deserializer<'de> for OptionTokenDeserializer<'a> {
    type Error = Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self.token {
            Some(s) => visitor.visit_borrowed_str(s),
            None => visitor.visit_none(),
        }
    }

    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self.token {
            Some(s) => visitor.visit_some(StrDeserializer(s)),
            None => visitor.visit_none(),
        }
    }

    fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self.token {
            Some(s) => {
                let val = s.parse::<u64>().map_err(|_| Error::InvalidType {
                    expected: "u64".to_string(),
                    got: s.to_string(),
                })?;
                visitor.visit_u64(val)
            }
            None => Err(Error::Custom("missing token".to_string())),
        }
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 u8 u16 u32 f32 f64 char str string bytes
        byte_buf unit unit_struct newtype_struct seq tuple
        tuple_struct map struct enum identifier ignored_any
    }
}

// ResponseBody deserializer
struct ResponseBodyDeserializer<'a> {
    body: &'a ParserResponseBody,
}

impl<'a> ResponseBodyDeserializer<'a> {
    fn new(body: &'a ParserResponseBody) -> Self {
        Self { body }
    }
}

impl<'de, 'a: 'de> de::Deserializer<'de> for ResponseBodyDeserializer<'a> {
    type Error = Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_enum("ResponseBody", &[], visitor)
    }

    fn deserialize_enum<V>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_enum(ResponseBodyEnumAccess::new(self.body))
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 u8 u16 u32 u64 f32 f64 char str string bytes
        byte_buf option unit unit_struct newtype_struct seq tuple
        tuple_struct map struct identifier ignored_any
    }
}

// Enum access for ResponseBody
struct ResponseBodyEnumAccess<'a> {
    body: &'a ParserResponseBody,
}

impl<'a> ResponseBodyEnumAccess<'a> {
    fn new(body: &'a ParserResponseBody) -> Self {
        Self { body }
    }
}

impl<'de, 'a: 'de> EnumAccess<'de> for ResponseBodyEnumAccess<'a> {
    type Error = Error;
    type Variant = ResponseBodyVariantAccess<'a>;

    fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Self::Variant), Self::Error>
    where
        V: DeserializeSeed<'de>,
    {
        match self.body {
            ParserResponseBody::Stream(stream) => {
                let variant_name = match stream.symbol {
                    StreamSymbol::Console => "Console",
                    StreamSymbol::TargetOutput => "TargetOutput",
                    StreamSymbol::InternalLogging => "InternalLogging",
                };
                let variant = seed.deserialize(StrDeserializer(variant_name))?;
                Ok((variant, ResponseBodyVariantAccess::Stream(&stream.text)))
            }
            ParserResponseBody::Data(data) => {
                let variant_name = match data.symbol {
                    DataSymbol::Result => "Result",
                    DataSymbol::AsyncExec => "AsyncExec",
                    DataSymbol::AsyncEnvironment => "AsyncEnvironment",
                    DataSymbol::AsyncStatus => "AsyncStatus",
                };
                let variant = seed.deserialize(StrDeserializer(variant_name))?;
                Ok((
                    variant,
                    ResponseBodyVariantAccess::Data(&data.class, &data.variables),
                ))
            }
        }
    }
}

enum ResponseBodyVariantAccess<'a> {
    Stream(&'a str),
    Data(&'a str, &'a HashMap<String, Value>),
}

impl<'de, 'a: 'de> VariantAccess<'de> for ResponseBodyVariantAccess<'a> {
    type Error = Error;

    fn unit_variant(self) -> Result<(), Self::Error> {
        Ok(())
    }

    fn newtype_variant_seed<T>(self, seed: T) -> Result<T::Value, Self::Error>
    where
        T: DeserializeSeed<'de>,
    {
        match self {
            ResponseBodyVariantAccess::Stream(s) => seed.deserialize(StrDeserializer(s)),
            ResponseBodyVariantAccess::Data(class, variables) => {
                seed.deserialize(DataDeserializer::from_data(class, variables))
            }
        }
    }

    fn tuple_variant<V>(self, _len: usize, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self {
            ResponseBodyVariantAccess::Stream(s) => {
                de::Deserializer::deserialize_seq(StrDeserializer(s), visitor)
            }
            ResponseBodyVariantAccess::Data(class, variables) => de::Deserializer::deserialize_seq(
                DataDeserializer::from_data(class, variables),
                visitor,
            ),
        }
    }

    fn struct_variant<V>(
        self,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self {
            ResponseBodyVariantAccess::Stream(s) => {
                de::Deserializer::deserialize_map(StrDeserializer(s), visitor)
            }
            ResponseBodyVariantAccess::Data(class, variables) => de::Deserializer::deserialize_map(
                DataDeserializer::from_data(class, variables),
                visitor,
            ),
        }
    }
}

#[cfg(test)]
mod tests {
    use serde::Deserialize;

    #[derive(Deserialize, PartialEq, Eq, Debug)]
    #[serde(rename_all = "kebab-case")]
    enum MyResponse {
        Foo(FooResponse),
        Bar,
        Baz { i: i8 },
    }

    #[derive(Deserialize, PartialEq, Eq, Debug)]
    #[serde(rename_all = "kebab-case")]
    struct FooResponse {
        bar: String,
        packet: FooPacket,
        metadata: Option<String>,
    }

    #[derive(Deserialize, PartialEq, Eq, Debug)]
    #[serde(rename_all = "kebab-case")]
    struct FooPacket {
        is_forwarded: u8,
        os: String,
        tags: Vec<PacketTag>,
    }

    #[derive(Deserialize, PartialEq, Eq, Debug)]
    #[serde(rename_all = "kebab-case")]
    enum PacketTag {
        Keepalive,
        Authorized,
        #[serde(other)]
        Other,
    }

    #[test]
    fn de_full_response() {
        assert_eq!(
            crate::from_str(r#"10^foo,bar="baz",packet={is-forwarded="1",os="oro",tags=["keepalive","authorized","yeet"]}"#).unwrap(),
            crate::Response {
                token: Some(10u64),
                body: crate::ResponseBody::Result(MyResponse::Foo(FooResponse {
                    bar: "baz".into(),
                    packet: FooPacket {
                        is_forwarded: 1,
                        os: "oro".into(),
                        tags: vec! [
                            PacketTag::Keepalive,
                            PacketTag::Authorized,
                            PacketTag::Other
                        ]
                    },
                    metadata: None,
                }))
            }
        );
    }

    #[test]
    fn de_extra_args() {
        // Extra args are OK, as long as we don't list any
        // args on the deserialized type that don't exist in the message.
        assert_eq!(
            crate::from_str(r#"*bar,foo="baz""#).unwrap(),
            crate::Response {
                token: None,
                body: crate::ResponseBody::AsyncExec(MyResponse::Bar)
            }
        );
    }

    #[test]
    fn de_convert_int() {
        assert_eq!(
            crate::from_str(r#"9988=baz,i="-42""#).unwrap(),
            crate::Response::<_, u64> {
                token: Some(9988u64),
                body: crate::ResponseBody::AsyncEnvironment(MyResponse::Baz { i: -42 })
            }
        );
    }
}
