using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using Abacus;
using Shouldly;
using Xunit;
using Xunit.Abstractions;

namespace tests
{
    public class AhoCorasickFixture
    {
        private readonly ITestOutputHelper output;
        static readonly Random Random = new Random();
        private const string Symbols = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";

        public AhoCorasickFixture(ITestOutputHelper output)
        {
            this.output = output;
        }
        
        [Fact]
        public void ExampleFromPaperIsSuccessful()
        {
            var keywords = new[] {"he", "she", "his", "hers"};
            
            var subject = new AhoCorasickStringMatch(keywords);
            var result = subject.Find("ushers").ToList();
            
            result.Count.ShouldBe(3);
            result.ShouldContain("she");
            result.ShouldContain("he");
            result.ShouldContain("hers");
        }

        [Fact]
        [Trait("Category", "Performance")]
        public void FindPerformanceIsLinearlyProportionalToSearchTextLength()
        {
            var stopwatch = new Stopwatch();
            var keywords = GenerateKeywords(100000);

            stopwatch.Start();
            var machine = new AhoCorasickStringMatch(keywords); 
            stopwatch.Stop();
            output.WriteLine($"Machine build time: {stopwatch.Elapsed.TotalMilliseconds}ms");
            
            // Burn one, to blow out the cobwebs 
            machine.Find("Foo");

            const int initialSearchTextLength = 1000;
            const double initialSearchTextAllowedMilliseconds = 0.025;

            var searchTextLengths = new[] {initialSearchTextLength, 10000, 100000, 1000000, 10000000 };

            // Limit to time growth of 3n 
            var searchTimesAllowed = searchTextLengths
                .Select(x => x == initialSearchTextLength
                    ? initialSearchTextAllowedMilliseconds
                    : (x / initialSearchTextLength) * initialSearchTextAllowedMilliseconds * 3).ToArray();
            
            for (var i = 0; i < searchTextLengths.Length; i++)
            {
                var searchText = GenerateRandomString(searchTextLengths[i]);
                stopwatch.Restart();
                machine.Find(searchText);
                stopwatch.Stop();
                var lookupTime = stopwatch.Elapsed;
                var allowedSearchTime = searchTimesAllowed[i];
                output.WriteLine($"Find time for search-text of length {searchTextLengths[i]}: {lookupTime.TotalMilliseconds}ms");
                output.WriteLine($"Allowed time: {allowedSearchTime}ms");
                
                lookupTime.TotalMilliseconds.ShouldBeLessThan(allowedSearchTime);
            }
        }

        [Fact]
        [Trait("Category", "Performance")]
        public void FindPerformanceIsNotProportionalToKeyWordCount()
        {
            var stopwatch = new Stopwatch();
            var keywordCounts = new[] { 1000, 100000 };
            var searchTimes = new long[keywordCounts.Length];
            var searchText = GenerateRandomString(100000);

            for (var i = 0; i < keywordCounts.Length; i++)
            {
                var machine = new AhoCorasickStringMatch(GenerateKeywords(keywordCounts[i]));
                machine.Find("foo"); // Burn one, to blow out the cobwebs
                stopwatch.Restart();
                machine.Find(searchText);
                stopwatch.Stop();
                var lookupTime = stopwatch.Elapsed;
                searchTimes[i] = lookupTime.Ticks;
                output.WriteLine($"Find time for machine with {keywordCounts[i]} keywords: {lookupTime.TotalMilliseconds}ms");
            }

            var smallTime = searchTimes.First();
            var bigTime = searchTimes.Last();

            if (bigTime > smallTime)
            {
                // Assert find time does not vary by more than 50%
                ((double)(bigTime - smallTime) / smallTime).ShouldBeLessThan(0.5);
            }
        }

        [Fact]
        [Trait("Category", "Performance")]
        public void TimeToConstructMachineGrowsLinearlyWithSumOfKeywordsLengths()
        {
            var stopwatch = new Stopwatch();
            
            const int initialKeywordCount = 1000;
            const int allowedInitialConstructionMilliseconds = 200;
            
            var keywordCounts = new[] { initialKeywordCount, 10000, 100000, 500000 };
            var constructionTimes = new long[keywordCounts.Length];
            
            // Limit to time growth of 2n 
            var constructionTimesAllowed = keywordCounts
                .Select(x => x == initialKeywordCount
                    ? allowedInitialConstructionMilliseconds
                    : (x / initialKeywordCount) * allowedInitialConstructionMilliseconds * 2).ToArray();
            
            for (var i = 0; i < keywordCounts.Length; i++)
            {
                stopwatch.Restart();
                var machine = new AhoCorasickStringMatch(GenerateKeywords(keywordCounts[i]));
                stopwatch.Stop();
                var constructionTime = stopwatch.Elapsed;
                constructionTimes[i] = constructionTime.Ticks;
                var allowedConstructionTime = constructionTimesAllowed[i];
                output.WriteLine($"Construction time for machine with {keywordCounts[i]} keywords: {constructionTime.TotalMilliseconds}ms");
                output.WriteLine($"Allowed time: {allowedConstructionTime}ms");
                constructionTime.TotalMilliseconds.ShouldBeLessThan(allowedConstructionTime);
            }
        }

        static IEnumerable<string> GenerateKeywords(int count)
        {
            for (var i = 0; i < count; i++)
            {
                var length = Random.Next(4, 50);
                yield return GenerateRandomString(length);
            }
        }

        static string GenerateRandomString(int length)
        {
            return new string(
                Enumerable.Range(1, length)
                    .Select(_ => Symbols[Random.Next(0, Symbols.Length - 1)])
                    .ToArray()
            );
        }
        
        
    }
}