using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;

namespace TdsItems
{
    public class SolutionProperties
    {
        public static string SolutionDirectory => GetSolutionDirectory();

        private static string GetSolutionDirectory()
        {
            var assemblyPath = new UriBuilder(Assembly.GetAssembly(typeof(SolutionProperties)).CodeBase).Path;
            var assemblyDirectory = Path.GetDirectoryName(assemblyPath) ?? string.Empty;
            return TrimFrom(new DirectoryInfo(assemblyDirectory).FullName, "\\src\\");
        }

        private static string TrimFrom(string str, string trimFromString)
        {
            var index = str.IndexOf(trimFromString, StringComparison.InvariantCulture);
            return index > 0 ? str.Remove(index) : str;
        }
    }
}
