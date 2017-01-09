using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using TdsParser;

namespace TdsItems
{
    public static class Project
    {
        private static readonly Dictionary<string, Dictionary<Guid, Types.Item>> Projects =
            new Dictionary<string, Dictionary<Guid, Types.Item>>();

        public static Dictionary<Guid, Types.Item> GetProject(string projectPath)
        {
            projectPath = Path.Combine(SolutionProperties.SolutionDirectory, projectPath);

            if (!Directory.Exists(projectPath))
            {
                throw new ArgumentException($"invalid project path '{projectPath}'");
            }
            return Projects.ContainsKey(projectPath)
                ? Projects[projectPath]
                : Projects[projectPath] = GetProjectItems(projectPath);
        }

        private static Dictionary<Guid, Types.Item> GetProjectItems(string projectPath)
        {
            var items = TdsParser.Project.GetProject(projectPath)
                .SelectMany(Flatten)
                .ToList();

            CheckDuplicates(items);

            return items.ToDictionary(d => d.Id, d => d);
        }

        private static void CheckDuplicates(IEnumerable<Types.Item> items)
        {
            var duplicates = items.GroupBy(i => i.Id).Where(g => g.Count() > 1).ToList();
            if (duplicates.Any())
            {
                throw new Exception($"duplicates found: '{string.Join(", ", duplicates.Select(g => g.Key))}'");
            }
        }

        private static IEnumerable<Types.Item> Flatten(Types.Item item)
        {
            return new[] { item }.Concat(item.Children.SelectMany(Flatten));
        }
    }
}
