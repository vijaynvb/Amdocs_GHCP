package com.example.todo.controller;

import com.example.todo.model.Todo;
import com.example.todo.repository.TodoRepository;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.net.URI;
import java.util.List;
import java.util.Optional;

@RestController
@RequestMapping("/api/todos")
public class TodoController {

    private final TodoRepository repository;

    public TodoController(TodoRepository repository) {
        this.repository = repository;
    }

    @GetMapping
    public List<Todo> list() {
        return repository.findAll();
    }

    @GetMapping("/{id}")
    public ResponseEntity<Todo> get(@PathVariable Long id) {
        return repository.findById(id)
                .map(ResponseEntity::ok)
                .orElse(ResponseEntity.notFound().build());
    }

    @PostMapping
    public ResponseEntity<Todo> create(@RequestBody Todo todo) {
        Todo saved = repository.save(todo);
        return ResponseEntity.created(URI.create("/api/todos/" + saved.getId())).body(saved);
    }

    @PutMapping("/{id}")
    public ResponseEntity<Todo> update(@PathVariable Long id, @RequestBody Todo todo) {
        Optional<Todo> existing = repository.findById(id);
        if (existing.isEmpty()) return ResponseEntity.notFound().build();
        Todo e = existing.get();
        e.setTitle(todo.getTitle());
        e.setCompleted(todo.isCompleted());
        repository.save(e);
        return ResponseEntity.ok(e);
    }

    @DeleteMapping("/{id}")
    public ResponseEntity<Void> delete(@PathVariable Long id) {
        if (!repository.existsById(id)) return ResponseEntity.notFound().build();
        repository.deleteById(id);
        return ResponseEntity.noContent().build();
    }
}
