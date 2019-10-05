using System.Linq;
using Abacus;
using Shouldly;
using Xunit;

namespace tests
{
    public class AhoCorasickFixture
    {
        [Fact]
        public void ExampleFromPaperIsSuccessful()
        {
            var keywords = new[] {"he", "she", "his", "hers"};
            
            var subject = new AhoCorasick(keywords);
            var result = subject.Find("ushers").ToList();
            
            result.Count.ShouldBe(3);
            result.ShouldContain("she");
            result.ShouldContain("he");
            result.ShouldContain("hers");
            
        }
    }
}