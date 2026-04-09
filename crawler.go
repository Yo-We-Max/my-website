// crawler.go — Concurrent web link crawler in Go
// Usage: go run crawler.go [start_url] [max_depth]
package main

import (
	"fmt"
	"net/http"
	"os"
	"regexp"
	"strconv"
	"strings"
	"sync"
	"time"
	"io"
)

var (
	visited = make(map[string]bool)
	mu      sync.Mutex
	wg      sync.WaitGroup
	linkRe  = regexp.MustCompile(`href=["'](https?://[^"'<>\s]+)["']`)
)

type Result struct {
	URL string; Status, Links, Depth int; Err error
}

func extractLinks(body string) (links []string) {
	for _, m := range linkRe.FindAllStringSubmatch(body, -1) {
		if link := strings.Split(m[1], "#")[0]; link != "" {
			links = append(links, link)
		}
	}
	return
}

func crawl(url string, depth, maxDepth int, results chan<- Result) {
	defer wg.Done()

	mu.Lock()
	if visited[url] || depth > maxDepth {
		mu.Unlock()
		return
	}
	visited[url] = true
	mu.Unlock()

	resp, err := (&http.Client{Timeout: 5 * time.Second}).Get(url)
	if err != nil {
		results <- Result{URL: url, Depth: depth, Err: err}
		return
	}
	defer resp.Body.Close()

	bodyBytes, err := io.ReadAll(io.LimitReader(resp.Body, 1<<20)) // 1MB limit
	if err != nil {
		results <- Result{URL: url, Status: resp.StatusCode, Depth: depth, Err: err}
		return
	}

	links := extractLinks(string(bodyBytes))
	results <- Result{URL: url, Status: resp.StatusCode, Links: len(links), Depth: depth}

	for _, link := range links {
		wg.Add(1)
		go crawl(link, depth+1, maxDepth, results)
	}
}

func main() {
	startURL, maxDepth := "https://go.dev", 1
	if len(os.Args) > 1 { startURL = os.Args[1] }
	if len(os.Args) > 2 { maxDepth, _ = strconv.Atoi(os.Args[2]) }

	fmt.Printf("Crawling %s (max depth: %d)...\n\n", startURL, maxDepth)
	results := make(chan Result, 100)

	wg.Add(1)
	go crawl(startURL, 0, maxDepth, results)

	go func() { wg.Wait(); close(results) }()

	count := 0
	for r := range results {
		count++
		if r.Err != nil {
			fmt.Printf("  [ERR] depth=%d %s\n", r.Depth, r.URL)
		} else {
			fmt.Printf("  [%d] depth=%d links=%-3d %s\n", r.Status, r.Depth, r.Links, r.URL)
		}
	}
	fmt.Printf("\nCrawled %d pages.\n", count)
}
