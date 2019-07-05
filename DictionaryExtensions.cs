using System.Linq;

// ReSharper disable once CheckNamespace
namespace System.Collections.Generic
{
    /// <summary>
    /// Linq extensions on the <see cref="IDictionary{TKey,TValue}"/> interface to work with dictionaries instead of <see cref="IEnumerable{T}"/> instances.
    /// </summary>
    public static class DictionaryExtensions
    {
        /// <summary>
        /// Creates an <see cref="IDictionary{TKey,TValue}"/> from an <see cref="IEnumerable{T}"/> that contains key/value pairs.
        /// </summary>
        public static IDictionary<TKey, TValue> ToDictionary<TKey, TValue>(this IEnumerable<KeyValuePair<TKey, TValue>> dictionary)
        {
            if (dictionary == null)
            {
                throw new ArgumentNullException(nameof(dictionary));
            }

            return dictionary.ToDictionary(kv => kv.Key, kv => kv.Value);
        }

        /// <summary>
        /// Projects each key/value pair of an <see cref="IDictionary{TKey,TValue}"/> into a new form.
        /// </summary>
        public static IDictionary<TKeyResult, TValueResult> Select<TKey, TValue, TKeyResult, TValueResult>(
            this IDictionary<TKey, TValue> dictionary,
            Func<KeyValuePair<TKey, TValue>, KeyValuePair<TKeyResult, TValueResult>> selector)
        {
            if (dictionary == null)
            {
                throw new ArgumentNullException(nameof(dictionary));
            }

            if (selector == null)
            {
                throw new ArgumentNullException(nameof(selector));
            }

            IEnumerable<KeyValuePair<TKeyResult, TValueResult>> results = Enumerable.Select(dictionary, selector);
            return results.ToDictionary();
        }

        /// <summary>
        /// Projects each key/value pair of an <see cref="IDictionary{TKey,TValue}"/> into a new form.
        /// </summary>
        public static IDictionary<TKeyResult, TValueResult> Select<TKey, TValue, TKeyResult, TValueResult>(
            this IDictionary<TKey, TValue> dictionary,
            Func<KeyValuePair<TKey, TValue>, int, KeyValuePair<TKeyResult, TValueResult>> selector)
        {
            if (dictionary == null)
            {
                throw new ArgumentNullException(nameof(dictionary));
            }

            if (selector == null)
            {
                throw new ArgumentNullException(nameof(selector));
            }

            IEnumerable<KeyValuePair<TKeyResult, TValueResult>> results = Enumerable.Select(dictionary, selector);
            return results.ToDictionary();
        }

        /// <summary>
        /// Projects each key/value pair of an <see cref="IDictionary{TKey,TValue}"/> into a new form.
        /// </summary>
        public static IDictionary<TKeyResult, TValueResult> SelectMany<TKey, TValue, TKeyResult, TValueResult>(
            this IDictionary<TKey, TValue> dictionary,
            Func<KeyValuePair<TKey, TValue>, IDictionary<TKeyResult, TValueResult>> selector)
        {
            if (dictionary == null)
            {
                throw new ArgumentNullException(nameof(dictionary));
            }

            if (selector == null)
            {
                throw new ArgumentNullException(nameof(selector));
            }

            IEnumerable<KeyValuePair<TKeyResult, TValueResult>> results = Enumerable.SelectMany(dictionary, selector);
            return results.ToDictionary();
        }
    }
}
